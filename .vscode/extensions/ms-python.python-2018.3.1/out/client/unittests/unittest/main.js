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
const configSettings_1 = require("../../common/configSettings");
const types_1 = require("../../common/types");
const baseTestManager_1 = require("../common/managers/baseTestManager");
const types_2 = require("../common/types");
const runner_1 = require("./runner");
class TestManager extends baseTestManager_1.BaseTestManager {
    get enabled() {
        return configSettings_1.PythonSettings.getInstance(this.workspaceFolder).unitTest.unittestEnabled;
    }
    constructor(workspaceFolder, rootDirectory, serviceContainer) {
        super('unittest', types_1.Product.unittest, workspaceFolder, rootDirectory, serviceContainer);
    }
    // tslint:disable-next-line:no-empty
    configure() {
    }
    getDiscoveryOptions(ignoreCache) {
        const args = this.settings.unitTest.unittestArgs.slice(0);
        return {
            workspaceFolder: this.workspaceFolder,
            cwd: this.rootDirectory, args,
            token: this.testDiscoveryCancellationToken, ignoreCache,
            outChannel: this.outputChannel
        };
    }
    runTestImpl(tests, testsToRun, runFailedTests, debug) {
        return __awaiter(this, void 0, void 0, function* () {
            const args = this.settings.unitTest.unittestArgs.slice(0);
            if (runFailedTests === true) {
                testsToRun = { testFile: [], testFolder: [], testSuite: [], testFunction: [] };
                testsToRun.testFunction = tests.testFunctions.filter(fn => {
                    return fn.testFunction.status === types_2.TestStatus.Error || fn.testFunction.status === types_2.TestStatus.Fail;
                }).map(fn => fn.testFunction);
            }
            const options = {
                workspaceFolder: this.workspaceFolder,
                cwd: this.rootDirectory,
                tests, args, testsToRun, debug,
                token: this.testRunnerCancellationToken,
                outChannel: this.outputChannel
            };
            return runner_1.runTest(this.serviceContainer, this, this.testResultsService, options);
        });
    }
}
exports.TestManager = TestManager;
//# sourceMappingURL=main.js.map