'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const helpers_1 = require("../../common/helpers");
const runner_1 = require("../common/runner");
const types_1 = require("../common/types");
const xUnitParser_1 = require("../common/xUnitParser");
const WITH_XUNIT = '--with-xunit';
const XUNIT_FILE = '--xunit-file';
// tslint:disable-next-line:no-any
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
    // tslint:disable-next-line:no-empty
    let xmlLogFileCleanup = () => { };
    // Check if '--with-xunit' is in args list
    const noseTestArgs = options.args.slice();
    if (noseTestArgs.indexOf(WITH_XUNIT) === -1) {
        noseTestArgs.push(WITH_XUNIT);
    }
    // Check if '--xunit-file' exists, if not generate random xml file
    const indexOfXUnitFile = noseTestArgs.findIndex(value => value.indexOf(XUNIT_FILE) === 0);
    let promiseToGetXmlLogFile;
    if (indexOfXUnitFile === -1) {
        promiseToGetXmlLogFile = helpers_1.createTemporaryFile('.xml').then(xmlLogResult => {
            xmlLogFileCleanup = xmlLogResult.cleanupCallback;
            xmlLogFile = xmlLogResult.filePath;
            noseTestArgs.push(`${XUNIT_FILE}=${xmlLogFile}`);
            return xmlLogResult.filePath;
        });
    }
    else {
        if (noseTestArgs[indexOfXUnitFile].indexOf('=') === -1) {
            xmlLogFile = noseTestArgs[indexOfXUnitFile + 1];
        }
        else {
            xmlLogFile = noseTestArgs[indexOfXUnitFile].substring(noseTestArgs[indexOfXUnitFile].indexOf('=') + 1).trim();
        }
        promiseToGetXmlLogFile = Promise.resolve(xmlLogFile);
    }
    return promiseToGetXmlLogFile.then(() => {
        if (options.debug === true) {
            const debugLauncher = serviceContainer.get(types_1.ITestDebugLauncher);
            const nosetestlauncherargs = [options.cwd, 'nose'];
            const debuggerArgs = nosetestlauncherargs.concat(noseTestArgs.concat(testPaths));
            const launchOptions = { cwd: options.cwd, args: debuggerArgs, token: options.token, outChannel: options.outChannel, testProvider: 'nosetest' };
            // tslint:disable-next-line:prefer-type-cast no-any
            return debugLauncher.launchDebugger(launchOptions);
        }
        else {
            // tslint:disable-next-line:prefer-type-cast no-any
            const runOptions = {
                args: noseTestArgs.concat(testPaths),
                cwd: options.cwd,
                outChannel: options.outChannel,
                token: options.token,
                workspaceFolder: options.workspaceFolder
            };
            return runner_1.run(serviceContainer, 'nosetest', runOptions);
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
// tslint:disable-next-line:no-any
function updateResultsFromLogFiles(tests, outputXmlFile, testResultsService) {
    return xUnitParser_1.updateResultsFromXmlLogFile(tests, outputXmlFile, xUnitParser_1.PassCalculationFormulae.nosetests).then(() => {
        testResultsService.updateResults(tests);
        return tests;
    });
}
exports.updateResultsFromLogFiles = updateResultsFromLogFiles;
//# sourceMappingURL=runner.js.map