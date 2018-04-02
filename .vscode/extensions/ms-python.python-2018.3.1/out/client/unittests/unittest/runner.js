'use strict';
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const path = require("path");
const runner_1 = require("../common/runner");
const types_1 = require("../common/types");
const outcomeMapping = new Map();
// tslint:disable-next-line:no-backbone-get-set-outside-model
outcomeMapping.set('passed', { status: types_1.TestStatus.Pass, summaryProperty: 'passed' });
// tslint:disable-next-line:no-backbone-get-set-outside-model
outcomeMapping.set('failed', { status: types_1.TestStatus.Fail, summaryProperty: 'failures' });
// tslint:disable-next-line:no-backbone-get-set-outside-model
outcomeMapping.set('error', { status: types_1.TestStatus.Error, summaryProperty: 'errors' });
// tslint:disable-next-line:no-backbone-get-set-outside-model
outcomeMapping.set('skipped', { status: types_1.TestStatus.Skipped, summaryProperty: 'skipped' });
// tslint:disable-next-line:max-func-body-length
function runTest(serviceContainer, testManager, testResultsService, options) {
    return __awaiter(this, void 0, void 0, function* () {
        options.tests.summary.errors = 0;
        options.tests.summary.failures = 0;
        options.tests.summary.passed = 0;
        options.tests.summary.skipped = 0;
        let failFast = false;
        const testLauncherFile = path.join(__dirname, '..', '..', '..', '..', 'pythonFiles', 'PythonTools', 'visualstudio_py_testlauncher.py');
        const server = serviceContainer.get(types_1.IUnitTestSocketServer);
        server.on('error', (message, ...data) => {
            // tslint:disable-next-line:no-console
            console.log(`${message} ${data.join(' ')}`);
        });
        // tslint:disable-next-line:no-empty
        server.on('log', (message, ...data) => {
        });
        // tslint:disable-next-line:no-empty no-any
        server.on('connect', (data) => {
        });
        // tslint:disable-next-line:no-empty
        server.on('start', (data) => {
        });
        server.on('result', (data) => {
            const test = options.tests.testFunctions.find(t => t.testFunction.nameToRun === data.test);
            const statusDetails = outcomeMapping.get(data.outcome);
            if (test) {
                test.testFunction.status = statusDetails.status;
                test.testFunction.message = data.message;
                test.testFunction.traceback = data.traceback;
                options.tests.summary[statusDetails.summaryProperty] += 1;
                if (failFast && (statusDetails.summaryProperty === 'failures' || statusDetails.summaryProperty === 'errors')) {
                    testManager.stop();
                }
            }
            else {
                if (statusDetails) {
                    options.tests.summary[statusDetails.summaryProperty] += 1;
                }
            }
        });
        // tslint:disable-next-line:no-empty no-any
        server.on('socket.disconnected', (data) => {
        });
        return server.start().then(port => {
            const testPaths = getIdsOfTestsToRun(options.tests, options.testsToRun);
            for (let counter = 0; counter < testPaths.length; counter += 1) {
                testPaths[counter] = `-t${testPaths[counter].trim()}`;
            }
            const startTestDiscoveryDirectory = getStartDirectory(options.args);
            function runTestInternal(testFile = '', testId = '') {
                let testArgs = buildTestArgs(options.args);
                failFast = testArgs.indexOf('--uf') >= 0;
                testArgs = testArgs.filter(arg => arg !== '--uf');
                testArgs.push(`--result-port=${port}`);
                testArgs.push(`--us=${startTestDiscoveryDirectory}`);
                if (testId.length > 0) {
                    testArgs.push(`-t${testId}`);
                }
                if (testFile.length > 0) {
                    testArgs.push(`--testFile=${testFile}`);
                }
                if (options.debug === true) {
                    const debugLauncher = serviceContainer.get(types_1.ITestDebugLauncher);
                    testArgs.push(...['--debug']);
                    const launchOptions = { cwd: options.cwd, args: testArgs, token: options.token, outChannel: options.outChannel, testProvider: 'unittest' };
                    // tslint:disable-next-line:prefer-type-cast no-any
                    return debugLauncher.launchDebugger(launchOptions);
                }
                else {
                    // tslint:disable-next-line:prefer-type-cast no-any
                    const runOptions = {
                        args: [testLauncherFile].concat(testArgs),
                        cwd: options.cwd,
                        outChannel: options.outChannel,
                        token: options.token,
                        workspaceFolder: options.workspaceFolder
                    };
                    return runner_1.run(serviceContainer, 'unittest', runOptions);
                }
            }
            // Test everything
            if (testPaths.length === 0) {
                return runTestInternal();
            }
            // Ok, the ptvs test runner can only work with one test at a time
            let promise = Promise.resolve('');
            if (Array.isArray(options.testsToRun.testFile)) {
                options.testsToRun.testFile.forEach(testFile => {
                    // tslint:disable-next-line:prefer-type-cast no-any
                    promise = promise.then(() => runTestInternal(testFile.fullPath, testFile.nameToRun));
                });
            }
            if (Array.isArray(options.testsToRun.testSuite)) {
                options.testsToRun.testSuite.forEach(testSuite => {
                    const testFileName = options.tests.testSuites.find(t => t.testSuite === testSuite).parentTestFile.fullPath;
                    // tslint:disable-next-line:prefer-type-cast no-any
                    promise = promise.then(() => runTestInternal(testFileName, testSuite.nameToRun));
                });
            }
            if (Array.isArray(options.testsToRun.testFunction)) {
                options.testsToRun.testFunction.forEach(testFn => {
                    const testFileName = options.tests.testFunctions.find(t => t.testFunction === testFn).parentTestFile.fullPath;
                    // tslint:disable-next-line:prefer-type-cast no-any
                    promise = promise.then(() => runTestInternal(testFileName, testFn.nameToRun));
                });
            }
            // tslint:disable-next-line:prefer-type-cast no-any
            return promise;
        }).then(() => {
            testResultsService.updateResults(options.tests);
            return options.tests;
        }).catch(reason => {
            return Promise.reject(reason);
        });
    });
}
exports.runTest = runTest;
function getStartDirectory(args) {
    let startDirectory = '.';
    const indexOfStartDir = args.findIndex(arg => arg.indexOf('-s') === 0 || arg.indexOf('--start-directory') === 0);
    if (indexOfStartDir >= 0) {
        const startDir = args[indexOfStartDir].trim();
        if ((startDir.trim() === '-s' || startDir.trim() === '--start-directory') && args.length >= indexOfStartDir) {
            // Assume the next items is the directory
            startDirectory = args[indexOfStartDir + 1];
        }
        else {
            const lenToStartFrom = startDir.startsWith('-s') ? '-s'.length : '--start-directory'.length;
            startDirectory = startDir.substring(lenToStartFrom).trim();
            if (startDirectory.startsWith('=')) {
                startDirectory = startDirectory.substring(1);
            }
        }
    }
    return startDirectory;
}
function buildTestArgs(args) {
    const startTestDiscoveryDirectory = getStartDirectory(args);
    let pattern = 'test*.py';
    const indexOfPattern = args.findIndex(arg => arg.indexOf('-p') === 0 || arg.indexOf('--pattern') === 0);
    if (indexOfPattern >= 0) {
        const patternValue = args[indexOfPattern].trim();
        if ((patternValue.trim() === '-p' || patternValue.trim() === '--pattern') && args.length >= indexOfPattern) {
            // Assume the next items is the directory
            pattern = args[indexOfPattern + 1];
        }
        else {
            const lenToStartFrom = patternValue.startsWith('-p') ? '-p'.length : '--pattern'.length;
            pattern = patternValue.substring(lenToStartFrom).trim();
            if (pattern.startsWith('=')) {
                pattern = pattern.substring(1);
            }
        }
    }
    const failFast = args.some(arg => arg.trim() === '-f' || arg.trim() === '--failfast');
    const verbosity = args.some(arg => arg.trim().indexOf('-v') === 0) ? 2 : 1;
    const testArgs = [`--us=${startTestDiscoveryDirectory}`, `--up=${pattern}`, `--uvInt=${verbosity}`];
    if (failFast) {
        testArgs.push('--uf');
    }
    return testArgs;
}
function getIdsOfTestsToRun(tests, testsToRun) {
    const testIds = [];
    if (testsToRun && testsToRun.testFolder) {
        // Get test ids of files in these folders
        testsToRun.testFolder.map(folder => {
            tests.testFiles.forEach(f => {
                if (f.fullPath.startsWith(folder.name)) {
                    testIds.push(f.nameToRun);
                }
            });
        });
    }
    if (testsToRun && testsToRun.testFile) {
        testIds.push(...testsToRun.testFile.map(f => f.nameToRun));
    }
    if (testsToRun && testsToRun.testSuite) {
        testIds.push(...testsToRun.testSuite.map(f => f.nameToRun));
    }
    if (testsToRun && testsToRun.testFunction) {
        testIds.push(...testsToRun.testFunction.map(f => f.nameToRun));
    }
    return testIds;
}
//# sourceMappingURL=runner.js.map