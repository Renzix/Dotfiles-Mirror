'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const helpers_1 = require("../../common/helpers");
const runner_1 = require("../common/runner");
const types_1 = require("../common/types");
const xUnitParser_1 = require("../common/xUnitParser");
function runTest(serviceContainer, testResultsService, options) {
    let testPaths = [];
    if (options.testsToRun && options.testsToRun.testFolder) {
        testPaths = testPaths.concat(options.testsToRun.testFolder.map(f => f.nameToRun));
    }
    if (options.testsToRun && options.testsToRun.testFile) {
        testPaths = testPaths.concat(options.testsToRun.testFile.map(f => f.nameToRun));
    }
    if (options.testsToRun && options.testsToRun.testSuite) {
        testPaths = testPaths.concat(options.testsToRun.testSuite.map(f => f.nameToRun));
    }
    if (options.testsToRun && options.testsToRun.testFunction) {
        testPaths = testPaths.concat(options.testsToRun.testFunction.map(f => f.nameToRun));
    }
    let xmlLogFile = '';
    let xmlLogFileCleanup;
    let args = options.args;
    return helpers_1.createTemporaryFile('.xml').then(xmlLogResult => {
        xmlLogFile = xmlLogResult.filePath;
        xmlLogFileCleanup = xmlLogResult.cleanupCallback;
        if (testPaths.length > 0) {
            // Ignore the test directories, as we're running a specific test
            args = args.filter(arg => arg.trim().startsWith('-'));
        }
        const testArgs = testPaths.concat(args, [`--junitxml=${xmlLogFile}`]);
        if (options.debug) {
            const debugLauncher = serviceContainer.get(types_1.ITestDebugLauncher);
            const pytestlauncherargs = [options.cwd, 'pytest'];
            const debuggerArgs = pytestlauncherargs.concat(testArgs);
            const launchOptions = { cwd: options.cwd, args: debuggerArgs, token: options.token, outChannel: options.outChannel, testProvider: 'pytest' };
            // tslint:disable-next-line:prefer-type-cast no-any
            return debugLauncher.launchDebugger(launchOptions);
        }
        else {
            const runOptions = {
                args: testArgs,
                cwd: options.cwd,
                outChannel: options.outChannel,
                token: options.token,
                workspaceFolder: options.workspaceFolder
            };
            return runner_1.run(serviceContainer, 'pytest', runOptions);
        }
    }).then(() => {
        return options.debug ? options.tests : updateResultsFromLogFiles(options.tests, xmlLogFile, testResultsService);
    }).then(result => {
        xmlLogFileCleanup();
        return result;
    }).catch(reason => {
        xmlLogFileCleanup();
        return Promise.reject(reason);
    });
}
exports.runTest = runTest;
function updateResultsFromLogFiles(tests, outputXmlFile, testResultsService) {
    return xUnitParser_1.updateResultsFromXmlLogFile(tests, outputXmlFile, xUnitParser_1.PassCalculationFormulae.pytest).then(() => {
        testResultsService.updateResults(tests);
        return tests;
    });
}
exports.updateResultsFromLogFiles = updateResultsFromLogFiles;
//# sourceMappingURL=runner.js.map