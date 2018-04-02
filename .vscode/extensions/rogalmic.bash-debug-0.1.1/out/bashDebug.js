"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_debugadapter_1 = require("vscode-debugadapter");
const child_process_1 = require("child_process");
const path_1 = require("path");
const which = require("npm-which");
const bashRuntime_1 = require("./bashRuntime");
const handlePath_1 = require("./handlePath");
class BashDebugSession extends vscode_debugadapter_1.LoggingDebugSession {
    constructor() {
        super("bash-debug.txt");
        this.currentBreakpointIds = new Map();
        this.fullDebugOutput = [""];
        this.fullDebugOutputIndex = 0;
        this.debuggerExecutableBusy = false;
        this.debuggerExecutableClosing = false;
        this.responsivityFactor = 5;
        this.debuggerProcessParentId = -1;
        // https://github.com/Microsoft/BashOnWindows/issues/1489
        this.debugPipeIndex = (process.platform === "win32") ? 2 : 3;
        this.setDebuggerLinesStartAt1(true);
        this.setDebuggerColumnsStartAt1(true);
    }
    initializeRequest(response, args) {
        response.body = response.body || {};
        response.body.supportsConditionalBreakpoints = false;
        response.body.supportsConfigurationDoneRequest = false;
        response.body.supportsEvaluateForHovers = true;
        response.body.supportsStepBack = false;
        response.body.supportsSetVariable = false;
        this.sendResponse(response);
    }
    disconnectRequest(response, args) {
        this.debuggerExecutableBusy = false;
        this.debuggerProcess.on("exit", () => {
            this.debuggerExecutableClosing = true;
            this.sendResponse(response);
        });
        child_process_1.spawn("bash", ["-c", `${this.launchArgs.pathPkill} -KILL -P ${this.debuggerProcessParentId}`]);
    }
    launchRequest(response, args) {
        this.launchArgs = args;
        vscode_debugadapter_1.logger.setup(args.trace ? vscode_debugadapter_1.Logger.LogLevel.Verbose : vscode_debugadapter_1.Logger.LogLevel.Stop, false);
        if (process.platform === "win32") {
            args.cwd = `${handlePath_1.getWSLPath(args.cwd)}`;
            args.program = `${handlePath_1.getWSLPath(args.program)}`;
        }
        {
            const errorMessage = bashRuntime_1.validatePath(args.cwd, args.pathBash, args.pathBashdb, args.pathCat, args.pathMkfifo, args.pathPkill);
            if (errorMessage !== "") {
                response.success = false;
                response.message = errorMessage;
                this.sendResponse(response);
                return;
            }
        }
        if (process.platform === "darwin" && args.pathPkill === "pkill") {
            const pathPkill = which(__dirname).sync('pkill');
            if (pathPkill === "/usr/local/bin/pkill") {
                const url = "https://github.com/rogalmic/vscode-bash-debug/wiki/macOS:-avoid-use-of--usr-local-bin-pkill";
                const msg = `Using /usr/bin/pkill instead of /usr/local/bin/pkill (see ${url} for details)`;
                this.sendEvent(new vscode_debugadapter_1.OutputEvent(msg, 'stderr'));
                args.pathPkill = "/usr/bin/pkill";
            }
        }
        const fifo_path = "/tmp/vscode-bash-debug-fifo-" + (Math.floor(Math.random() * 10000) + 10000);
        // TODO: treat whitespace in args.args:
        //       i.e. at this moment, ["arg0", "arg1 with space"] will be expanded to "arg0 arg1 with space"
        // use fifo, because --tty '&1' does not work properly for subshell (when bashdb spawns - $() )
        // when this is fixed in bashdb, use &1
        this.debuggerProcess = child_process_1.spawn(args.pathBash, ["-c", `

			# http://tldp.org/LDP/abs/html/io-redirection.html

			function cleanup()
			{
				exit_code=$?
				trap '' ERR SIGINT SIGTERM EXIT
				exec 4>&-
				rm "${fifo_path}";
				exit $exit_code;
			}
			trap 'cleanup' ERR SIGINT SIGTERM EXIT

			mkfifo "${fifo_path}"
			${args.pathCat} "${fifo_path}" >&${this.debugPipeIndex} &
			exec 4>"${fifo_path}" 		# Keep open for writing, bashdb seems close after every write.
			cd ${args.cwd}
			${args.pathCat} | ${args.pathBashdb} --quiet --tty "${fifo_path}" -- "${args.program}" ${args.args.join(" ")}
			`
        ], { stdio: ["pipe", "pipe", "pipe", "pipe"] });
        this.debuggerProcess.on("error", (error) => {
            this.sendEvent(new vscode_debugadapter_1.OutputEvent(`${error}`, 'stderr'));
        });
        this.processDebugTerminalOutput();
        this.debuggerProcess.stdin.write(`print "$PPID"\nhandle INT stop\nprint "${BashDebugSession.END_MARKER}"\n`);
        this.debuggerProcess.stdio[1].on("data", (data) => {
            this.sendEvent(new vscode_debugadapter_1.OutputEvent(`${data}`, 'stdout'));
        });
        this.debuggerProcess.stdio[2].on("data", (data) => {
            this.sendEvent(new vscode_debugadapter_1.OutputEvent(`${data}`, 'stderr'));
        });
        this.debuggerProcess.stdio[3].on("data", (data) => {
            if (args.showDebugOutput) {
                this.sendEvent(new vscode_debugadapter_1.OutputEvent(`${data}`, 'console'));
            }
        });
        this.scheduleExecution(() => this.launchRequestFinalize(response, args));
    }
    launchRequestFinalize(response, args) {
        for (let i = 0; i < this.fullDebugOutput.length; i++) {
            if (this.fullDebugOutput[i] === BashDebugSession.END_MARKER) {
                this.debuggerProcessParentId = parseInt(this.fullDebugOutput[i - 1]);
                this.sendResponse(response);
                this.sendEvent(new vscode_debugadapter_1.OutputEvent(`Sending InitializedEvent`, 'telemetry'));
                this.sendEvent(new vscode_debugadapter_1.InitializedEvent());
                const interval = setInterval((data) => {
                    for (; this.fullDebugOutputIndex < this.fullDebugOutput.length - 1; this.fullDebugOutputIndex++) {
                        const line = this.fullDebugOutput[this.fullDebugOutputIndex];
                        if (line.indexOf("(/") === 0 && line.indexOf("):") === line.length - 2) {
                            this.sendEvent(new vscode_debugadapter_1.OutputEvent(`Sending StoppedEvent`, 'telemetry'));
                            this.sendEvent(new vscode_debugadapter_1.StoppedEvent("break", BashDebugSession.THREAD_ID));
                        }
                        else if (line.indexOf("terminated") > 0) {
                            clearInterval(interval);
                            this.debuggerProcess.stdin.write(`\nq\n`);
                            this.sendEvent(new vscode_debugadapter_1.OutputEvent(`Sending TerminatedEvent`, 'telemetry'));
                            this.sendEvent(new vscode_debugadapter_1.TerminatedEvent());
                        }
                    }
                }, this.responsivityFactor);
                return;
            }
        }
        this.scheduleExecution(() => this.launchRequestFinalize(response, args));
    }
    setBreakPointsRequest(response, args) {
        if (this.debuggerExecutableBusy) {
            this.scheduleExecution(() => this.setBreakPointsRequest(response, args));
            return;
        }
        if (!args.source.path) {
            this.sendEvent(new vscode_debugadapter_1.OutputEvent("Error: setBreakPointsRequest(): args.source.path is undefined.", 'stderr'));
            return;
        }
        if (!this.currentBreakpointIds[args.source.path]) {
            this.currentBreakpointIds[args.source.path] = [];
        }
        const sourcePath = (process.platform === "win32") ? handlePath_1.getWSLPath(args.source.path) : args.source.path;
        let setBreakpointsCommand = `print 'delete <${this.currentBreakpointIds[args.source.path].join(" ")}>'\ndelete ${this.currentBreakpointIds[args.source.path].join(" ")}\nload ${sourcePath}\n`;
        if (args.breakpoints) {
            args.breakpoints.forEach((b) => { setBreakpointsCommand += `print ' <${sourcePath}:${b.line}> '\nbreak ${sourcePath}:${b.line}\n`; });
        }
        this.debuggerExecutableBusy = true;
        const currentLine = this.fullDebugOutput.length;
        this.debuggerProcess.stdin.write(`${setBreakpointsCommand}print '${BashDebugSession.END_MARKER}'\n`);
        this.scheduleExecution(() => this.setBreakPointsRequestFinalize(response, args, currentLine));
    }
    setBreakPointsRequestFinalize(response, args, currentOutputLength) {
        if (!args.source.path) {
            this.sendEvent(new vscode_debugadapter_1.OutputEvent("Error: setBreakPointsRequestFinalize(): args.source.path is undefined.", 'stderr'));
            return;
        }
        if (this.promptReached(currentOutputLength)) {
            this.currentBreakpointIds[args.source.path] = [];
            const breakpoints = new Array();
            for (let i = currentOutputLength; i < this.fullDebugOutput.length - 2; i++) {
                if (this.fullDebugOutput[i - 1].indexOf(" <") === 0 && this.fullDebugOutput[i - 1].indexOf("> ") > 0) {
                    const lineNodes = this.fullDebugOutput[i].split(" ");
                    const bp = new vscode_debugadapter_1.Breakpoint(true, this.convertDebuggerLineToClient(parseInt(lineNodes[lineNodes.length - 1].replace(".", ""))));
                    bp.id = parseInt(lineNodes[1]);
                    breakpoints.push(bp);
                    this.currentBreakpointIds[args.source.path].push(bp.id);
                }
            }
            response.body = { breakpoints: breakpoints };
            this.debuggerExecutableBusy = false;
            this.sendResponse(response);
            return;
        }
        this.scheduleExecution(() => this.setBreakPointsRequestFinalize(response, args, currentOutputLength));
    }
    threadsRequest(response) {
        response.body = { threads: [new vscode_debugadapter_1.Thread(BashDebugSession.THREAD_ID, "Bash thread")] };
        this.sendResponse(response);
    }
    stackTraceRequest(response, args) {
        if (this.debuggerExecutableBusy) {
            this.scheduleExecution(() => this.stackTraceRequest(response, args));
            return;
        }
        this.debuggerExecutableBusy = true;
        const currentLine = this.fullDebugOutput.length;
        this.debuggerProcess.stdin.write(`print backtrace\nbacktrace\nprint '${BashDebugSession.END_MARKER}'\n`);
        this.scheduleExecution(() => this.stackTraceRequestFinalize(response, args, currentLine));
    }
    stackTraceRequestFinalize(response, args, currentOutputLength) {
        if (this.promptReached(currentOutputLength)) {
            const lastStackLineIndex = this.fullDebugOutput.length - 3;
            let frames = new Array();
            for (let i = currentOutputLength; i <= lastStackLineIndex; i++) {
                const lineContent = this.fullDebugOutput[i];
                const frameIndex = parseInt(lineContent.substr(2, 2));
                const frameText = lineContent;
                let frameSourcePath = lineContent.substr(lineContent.lastIndexOf("`") + 1, lineContent.lastIndexOf("'") - lineContent.lastIndexOf("`") - 1);
                const frameLine = parseInt(lineContent.substr(lineContent.lastIndexOf(" ")));
                if ((process.platform === "win32")) {
                    frameSourcePath = frameSourcePath.startsWith("/") ? handlePath_1.reverseWSLPath(frameSourcePath) : handlePath_1.reverseWSLPath(`${this.launchArgs.cwd}/${frameSourcePath}`);
                }
                frames.push(new vscode_debugadapter_1.StackFrame(frameIndex, frameText, new vscode_debugadapter_1.Source(path_1.basename(frameSourcePath), this.convertDebuggerPathToClient(frameSourcePath), undefined, undefined, 'bash-adapter-data'), this.convertDebuggerLineToClient(frameLine)));
            }
            const totalFrames = this.fullDebugOutput.length - currentOutputLength - 1;
            const startFrame = typeof args.startFrame === 'number' ? args.startFrame : 0;
            const maxLevels = typeof args.levels === 'number' ? args.levels : 100;
            frames = frames.slice(startFrame, Math.min(startFrame + maxLevels, frames.length));
            response.body = { stackFrames: frames, totalFrames: totalFrames };
            this.debuggerExecutableBusy = false;
            this.sendResponse(response);
            return;
        }
        this.scheduleExecution(() => this.stackTraceRequestFinalize(response, args, currentOutputLength));
    }
    scopesRequest(response, args) {
        const scopes = [new vscode_debugadapter_1.Scope("Local", this.fullDebugOutputIndex, false)];
        response.body = { scopes: scopes };
        this.sendResponse(response);
    }
    variablesRequest(response, args) {
        if (this.debuggerExecutableBusy) {
            this.scheduleExecution(() => this.variablesRequest(response, args));
            return;
        }
        let getVariablesCommand = `info program\n`;
        const count = typeof args.count === 'number' ? args.count : 100;
        const start = typeof args.start === 'number' ? args.start : 0;
        let variableDefinitions = ["PWD", "EUID", "#", "0", "-"];
        variableDefinitions = variableDefinitions.slice(start, Math.min(start + count, variableDefinitions.length));
        variableDefinitions.forEach((v) => { getVariablesCommand += `print ' <$${v}> '\nexamine $${v}\n`; });
        this.debuggerExecutableBusy = true;
        const currentLine = this.fullDebugOutput.length;
        this.debuggerProcess.stdin.write(`${getVariablesCommand}print '${BashDebugSession.END_MARKER}'\n`);
        this.scheduleExecution(() => this.variablesRequestFinalize(response, args, currentLine));
    }
    variablesRequestFinalize(response, args, currentOutputLength) {
        if (this.promptReached(currentOutputLength)) {
            let variables = [];
            for (let i = currentOutputLength; i < this.fullDebugOutput.length - 2; i++) {
                if (this.fullDebugOutput[i - 1].indexOf(" <") === 0 && this.fullDebugOutput[i - 1].indexOf("> ") > 0) {
                    variables.push({
                        name: `${this.fullDebugOutput[i - 1].replace(" <", "").replace("> ", "")}`,
                        type: "string",
                        value: this.fullDebugOutput[i],
                        variablesReference: 0
                    });
                }
            }
            response.body = { variables: variables };
            this.debuggerExecutableBusy = false;
            this.sendResponse(response);
            return;
        }
        this.scheduleExecution(() => this.variablesRequestFinalize(response, args, currentOutputLength));
    }
    continueRequest(response, args) {
        if (this.debuggerExecutableBusy) {
            this.scheduleExecution(() => this.continueRequest(response, args));
            return;
        }
        this.debuggerExecutableBusy = true;
        const currentLine = this.fullDebugOutput.length;
        this.debuggerProcess.stdin.write(`print continue\ncontinue\nprint '${BashDebugSession.END_MARKER}'\n`);
        this.scheduleExecution(() => this.continueRequestFinalize(response, args, currentLine));
        // NOTE: do not wait for step to finish
        this.sendResponse(response);
    }
    continueRequestFinalize(response, args, currentOutputLength) {
        if (this.promptReached(currentOutputLength)) {
            this.debuggerExecutableBusy = false;
            return;
        }
        this.scheduleExecution(() => this.continueRequestFinalize(response, args, currentOutputLength));
    }
    // bashdb doesn't support reverse execution
    // protected reverseContinueRequest(response: DebugProtocol.ReverseContinueResponse, args: DebugProtocol.ReverseContinueArguments) : void {
    // }
    nextRequest(response, args) {
        if (this.debuggerExecutableBusy) {
            this.scheduleExecution(() => this.nextRequest(response, args));
            return;
        }
        this.debuggerExecutableBusy = true;
        const currentLine = this.fullDebugOutput.length;
        this.debuggerProcess.stdin.write(`print next\nnext\nprint '${BashDebugSession.END_MARKER}'\n`);
        this.scheduleExecution(() => this.nextRequestFinalize(response, args, currentLine));
        // NOTE: do not wait for step to finish
        this.sendResponse(response);
    }
    nextRequestFinalize(response, args, currentOutputLength) {
        if (this.promptReached(currentOutputLength)) {
            this.debuggerExecutableBusy = false;
            return;
        }
        this.scheduleExecution(() => this.nextRequestFinalize(response, args, currentOutputLength));
    }
    stepInRequest(response, args) {
        if (this.debuggerExecutableBusy) {
            this.scheduleExecution(() => this.stepInRequest(response, args));
            return;
        }
        this.debuggerExecutableBusy = true;
        const currentLine = this.fullDebugOutput.length;
        this.debuggerProcess.stdin.write(`print step\nstep\nprint '${BashDebugSession.END_MARKER}'\n`);
        this.scheduleExecution(() => this.stepInRequestFinalize(response, args, currentLine));
        // NOTE: do not wait for step to finish
        this.sendResponse(response);
    }
    stepInRequestFinalize(response, args, currentOutputLength) {
        if (this.promptReached(currentOutputLength)) {
            this.debuggerExecutableBusy = false;
            return;
        }
        this.scheduleExecution(() => this.stepInRequestFinalize(response, args, currentOutputLength));
    }
    stepOutRequest(response, args) {
        if (this.debuggerExecutableBusy) {
            this.scheduleExecution(() => this.stepOutRequest(response, args));
            return;
        }
        this.debuggerExecutableBusy = true;
        const currentLine = this.fullDebugOutput.length;
        this.debuggerProcess.stdin.write(`print finish\nfinish\nprint '${BashDebugSession.END_MARKER}'\n`);
        this.scheduleExecution(() => this.stepOutRequestFinalize(response, args, currentLine));
        // NOTE: do not wait for step to finish
        this.sendResponse(response);
    }
    stepOutRequestFinalize(response, args, currentOutputLength) {
        if (this.promptReached(currentOutputLength)) {
            this.debuggerExecutableBusy = false;
            return;
        }
        this.scheduleExecution(() => this.stepOutRequestFinalize(response, args, currentOutputLength));
    }
    // bashdb doesn't support reverse execution
    // protected stepBackRequest(response: DebugProtocol.StepBackResponse, args: DebugProtocol.StepBackArguments): void {
    // }
    evaluateRequest(response, args) {
        if (this.debuggerProcess === null) {
            response.body = { result: `${args.expression} = ''`, variablesReference: 0 };
            this.debuggerExecutableBusy = false;
            this.sendResponse(response);
            return;
        }
        if (this.debuggerExecutableBusy) {
            this.scheduleExecution(() => this.evaluateRequest(response, args));
            return;
        }
        this.debuggerExecutableBusy = true;
        const currentLine = this.fullDebugOutput.length;
        this.debuggerProcess.stdin.write(`print 'examine <${args.expression}>'\nexamine ${args.expression.replace("\"", "")}\nprint '${BashDebugSession.END_MARKER}'\n`);
        this.scheduleExecution(() => this.evaluateRequestFinalize(response, args, currentLine));
    }
    evaluateRequestFinalize(response, args, currentOutputLength) {
        if (this.promptReached(currentOutputLength)) {
            response.body = { result: `'${this.fullDebugOutput[currentOutputLength]}'`, variablesReference: 0 };
            this.debuggerExecutableBusy = false;
            this.sendResponse(response);
            return;
        }
        this.scheduleExecution(() => this.evaluateRequestFinalize(response, args, currentOutputLength));
    }
    pauseRequest(response, args) {
        if (args.threadId === BashDebugSession.THREAD_ID) {
            child_process_1.spawn("bash", ["-c", `${this.launchArgs.pathPkill} -INT -P ${this.debuggerProcessParentId} -f bashdb`]).on("exit", () => this.sendResponse(response));
            return;
        }
        response.success = false;
        this.sendResponse(response);
    }
    removePrompt(line) {
        if (line.indexOf("bashdb<") === 0) {
            return line.substr(line.indexOf("> ") + 2);
        }
        return line;
    }
    promptReached(currentOutputLength) {
        return this.fullDebugOutput.length > currentOutputLength && this.fullDebugOutput[this.fullDebugOutput.length - 2] === BashDebugSession.END_MARKER;
    }
    processDebugTerminalOutput() {
        this.debuggerProcess.stdio[this.debugPipeIndex].on('data', (data) => {
            if (this.fullDebugOutput.length === 1 && data.indexOf("Reading ") === 0) {
                // Before debug run, there is no newline
                return;
            }
            const list = data.toString().split("\n", -1);
            const fullLine = `${this.fullDebugOutput.pop()}${list.shift()}`;
            this.fullDebugOutput.push(this.removePrompt(fullLine));
            list.forEach(l => this.fullDebugOutput.push(this.removePrompt(l)));
        });
    }
    scheduleExecution(callback) {
        if (!this.debuggerExecutableClosing) {
            setTimeout(() => callback(), this.responsivityFactor);
        }
    }
}
BashDebugSession.THREAD_ID = 42;
BashDebugSession.END_MARKER = "############################################################";
exports.BashDebugSession = BashDebugSession;
vscode_debugadapter_1.DebugSession.run(BashDebugSession);
//# sourceMappingURL=bashDebug.js.map