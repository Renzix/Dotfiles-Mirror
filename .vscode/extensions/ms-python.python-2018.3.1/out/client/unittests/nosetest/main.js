"use strict";
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __param = (this && this.__param) || function (paramIndex, decorator) {
    return function (target, key) { decorator(target, key, paramIndex); }
};
Object.defineProperty(exports, "__esModule", { value: true });
const inversify_1 = require("inversify");
const vscode_1 = require("vscode");
const configSettings_1 = require("../../common/configSettings");
const types_1 = require("../../common/types");
const types_2 = require("../../ioc/types");
const baseTestManager_1 = require("../common/managers/baseTestManager");
const runner_1 = require("./runner");
let TestManager = class TestManager extends baseTestManager_1.BaseTestManager {
    constructor(workspaceFolder, rootDirectory, serviceContainer) {
        super('nosetest', types_1.Product.nosetest, workspaceFolder, rootDirectory, serviceContainer);
    }
    get enabled() {
        return configSettings_1.PythonSettings.getInstance(this.workspaceFolder).unitTest.nosetestsEnabled;
    }
    getDiscoveryOptions(ignoreCache) {
        const args = this.settings.unitTest.nosetestArgs.slice(0);
        return {
            workspaceFolder: this.workspaceFolder,
            cwd: this.rootDirectory, args,
            token: this.testDiscoveryCancellationToken, ignoreCache,
            outChannel: this.outputChannel
        };
    }
    // tslint:disable-next-line:no-any
    runTestImpl(tests, testsToRun, runFailedTests, debug) {
        const args = this.settings.unitTest.nosetestArgs.slice(0);
        if (runFailedTests === true && args.indexOf('--failed') === -1) {
            args.push('--failed');
        }
        if (!runFailedTests && args.indexOf('--with-id') === -1) {
            args.push('--with-id');
        }
        const options = {
            workspaceFolder: vscode_1.Uri.file(this.rootDirectory),
            cwd: this.rootDirectory,
            tests, args, testsToRun,
            token: this.testRunnerCancellationToken,
            outChannel: this.outputChannel,
            debug
        };
        return runner_1.runTest(this.serviceContainer, this.testResultsService, options);
    }
};
TestManager = __decorate([
    inversify_1.injectable(),
    __param(2, inversify_1.inject(types_2.IServiceContainer))
], TestManager);
exports.TestManager = TestManager;
//# sourceMappingURL=main.js.map