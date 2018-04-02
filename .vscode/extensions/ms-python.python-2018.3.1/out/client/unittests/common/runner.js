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
const path = require("path");
const configSettings_1 = require("../../common/configSettings");
const errorUtils_1 = require("../../common/errors/errorUtils");
const moduleNotInstalledError_1 = require("../../common/errors/moduleNotInstalledError");
const types_1 = require("../../common/process/types");
const types_2 = require("../../common/process/types");
const constants_1 = require("./constants");
const types_3 = require("./types");
function run(serviceContainer, testProvider, options) {
    return __awaiter(this, void 0, void 0, function* () {
        const testExecutablePath = getExecutablePath(testProvider, configSettings_1.PythonSettings.getInstance(options.workspaceFolder));
        const moduleName = getTestModuleName(testProvider);
        const spawnOptions = options;
        let pythonExecutionServicePromise;
        spawnOptions.mergeStdOutErr = typeof spawnOptions.mergeStdOutErr === 'boolean' ? spawnOptions.mergeStdOutErr : true;
        let promise;
        if (!testExecutablePath && testProvider === constants_1.UNITTEST_PROVIDER) {
            // Unit tests have a special way of being executed
            const pythonServiceFactory = serviceContainer.get(types_2.IPythonExecutionFactory);
            pythonExecutionServicePromise = pythonServiceFactory.create(options.workspaceFolder);
            promise = pythonExecutionServicePromise.then(executionService => executionService.execObservable(options.args, Object.assign({}, spawnOptions)));
        }
        else {
            const pythonToolsExecutionService = serviceContainer.get(types_1.IPythonToolExecutionService);
            const testHelper = serviceContainer.get(types_3.ITestsHelper);
            const executionInfo = {
                execPath: testExecutablePath,
                args: options.args,
                moduleName: testExecutablePath && testExecutablePath.length > 0 ? undefined : moduleName,
                product: testHelper.parseProduct(testProvider)
            };
            promise = pythonToolsExecutionService.execObservable(executionInfo, spawnOptions, options.workspaceFolder);
        }
        return promise.then(result => {
            return new Promise((resolve, reject) => {
                let stdOut = '';
                let stdErr = '';
                result.out.subscribe(output => {
                    stdOut += output.out;
                    // If the test runner python module is not installed we'll have something in stderr.
                    // Hence track that separately and check at the end.
                    if (output.source === 'stderr') {
                        stdErr += output.out;
                    }
                    if (options.outChannel) {
                        options.outChannel.append(output.out);
                    }
                }, reject, () => __awaiter(this, void 0, void 0, function* () {
                    // If the test runner python module is not installed we'll have something in stderr.
                    if (moduleName && pythonExecutionServicePromise && errorUtils_1.ErrorUtils.outputHasModuleNotInstalledError(moduleName, stdErr)) {
                        const pythonExecutionService = yield pythonExecutionServicePromise;
                        const isInstalled = yield pythonExecutionService.isModuleInstalled(moduleName);
                        if (!isInstalled) {
                            return reject(new moduleNotInstalledError_1.ModuleNotInstalledError(moduleName));
                        }
                    }
                    resolve(stdOut);
                }));
            });
        });
    });
}
exports.run = run;
function getExecutablePath(testProvider, settings) {
    let testRunnerExecutablePath;
    switch (testProvider) {
        case constants_1.NOSETEST_PROVIDER: {
            testRunnerExecutablePath = settings.unitTest.nosetestPath;
            break;
        }
        case constants_1.PYTEST_PROVIDER: {
            testRunnerExecutablePath = settings.unitTest.pyTestPath;
            break;
        }
        default: {
            return undefined;
        }
    }
    return path.basename(testRunnerExecutablePath) === testRunnerExecutablePath ? undefined : testRunnerExecutablePath;
}
function getTestModuleName(testProvider) {
    switch (testProvider) {
        case constants_1.NOSETEST_PROVIDER: {
            return 'nose';
        }
        case constants_1.PYTEST_PROVIDER: {
            return 'pytest';
        }
        case constants_1.UNITTEST_PROVIDER: {
            return 'unittest';
        }
        default: {
            throw new Error(`Test provider '${testProvider}' not supported`);
        }
    }
}
//# sourceMappingURL=runner.js.map