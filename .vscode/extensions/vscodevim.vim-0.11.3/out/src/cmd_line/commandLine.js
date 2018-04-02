"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const configuration_1 = require("../configuration/configuration");
const statusBar_1 = require("../statusBar");
const parser = require("./parser");
const util = require("../util");
const error_1 = require("../error");
class CommandLine {
    static PromptAndRun(initialText, vimState) {
        return __awaiter(this, void 0, void 0, function* () {
            if (!vscode.window.activeTextEditor) {
                console.log('CommandLine: No active document.');
                return;
            }
            let cmd = yield vscode.window.showInputBox(this.getInputBoxOptions(initialText));
            if (cmd && cmd[0] === ':' && configuration_1.configuration.cmdLineInitialColon) {
                cmd = cmd.slice(1);
            }
            yield CommandLine.Run(cmd, vimState);
        });
    }
    static Run(command, vimState) {
        return __awaiter(this, void 0, void 0, function* () {
            if (!command || command.length === 0) {
                return;
            }
            try {
                const cmd = parser.parse(command);
                const useNeovim = configuration_1.configuration.enableNeovim && cmd.command && cmd.command.neovimCapable;
                if (useNeovim) {
                    yield vimState.nvim.run(vimState, command);
                }
                else {
                    yield cmd.execute(vimState.editor, vimState);
                }
            }
            catch (e) {
                if (e instanceof error_1.VimError) {
                    if (e.code === error_1.ErrorCode.E492 && configuration_1.configuration.enableNeovim) {
                        yield vimState.nvim.run(vimState, command);
                    }
                    else {
                        statusBar_1.StatusBar.SetText(`${e.toString()}. ${command}`, vimState.currentMode, vimState.isRecordingMacro, true);
                    }
                }
                else {
                    util.showError(e.toString());
                }
            }
        });
    }
    static getInputBoxOptions(text) {
        return {
            prompt: 'Vim command line',
            value: configuration_1.configuration.cmdLineInitialColon ? ':' + text : text,
            ignoreFocusOut: false,
            valueSelection: [
                configuration_1.configuration.cmdLineInitialColon ? text.length + 1 : text.length,
                configuration_1.configuration.cmdLineInitialColon ? text.length + 1 : text.length,
            ],
        };
    }
}
exports.CommandLine = CommandLine;

//# sourceMappingURL=commandLine.js.map
