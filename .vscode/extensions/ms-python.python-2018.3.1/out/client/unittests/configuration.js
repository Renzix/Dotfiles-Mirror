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
const vscode = require("vscode");
const configSettings_1 = require("../common/configSettings");
const types_1 = require("../common/types");
const utils_1 = require("../common/utils");
const configSettingService_1 = require("./common/services/configSettingService");
const nose = require("./nosetest/testConfigurationManager");
const pytest = require("./pytest/testConfigurationManager");
const unittest = require("./unittest/testConfigurationManager");
// tslint:disable-next-line:no-any
function promptToEnableAndConfigureTestFramework(wkspace, installer, outputChannel, messageToDisplay = 'Select a test framework/tool to enable', enableOnly = false) {
    return __awaiter(this, void 0, void 0, function* () {
        const selectedTestRunner = yield selectTestRunner(messageToDisplay);
        if (typeof selectedTestRunner !== 'number') {
            return Promise.reject(null);
        }
        const configMgr = createTestConfigurationManager(wkspace, selectedTestRunner, outputChannel, installer);
        if (enableOnly) {
            // Ensure others are disabled
            [types_1.Product.unittest, types_1.Product.pytest, types_1.Product.nosetest]
                .filter(prod => selectedTestRunner !== prod)
                .forEach(prod => {
                createTestConfigurationManager(wkspace, prod, outputChannel, installer).disable()
                    .catch(ex => console.error('Python Extension: createTestConfigurationManager.disable', ex));
            });
            return configMgr.enable();
        }
        return configMgr.configure(wkspace).then(() => {
            return enableTest(wkspace, configMgr);
        }).catch(reason => {
            return enableTest(wkspace, configMgr).then(() => Promise.reject(reason));
        });
    });
}
function displayTestFrameworkError(wkspace, outputChannel, installer) {
    const settings = configSettings_1.PythonSettings.getInstance();
    let enabledCount = settings.unitTest.pyTestEnabled ? 1 : 0;
    enabledCount += settings.unitTest.nosetestsEnabled ? 1 : 0;
    enabledCount += settings.unitTest.unittestEnabled ? 1 : 0;
    if (enabledCount > 1) {
        return promptToEnableAndConfigureTestFramework(wkspace, installer, outputChannel, 'Enable only one of the test frameworks (unittest, pytest or nosetest).', true);
    }
    else {
        const option = 'Enable and configure a Test Framework';
        return vscode.window.showInformationMessage('No test framework configured (unittest, pytest or nosetest)', option).then(item => {
            if (item === option) {
                return promptToEnableAndConfigureTestFramework(wkspace, installer, outputChannel);
            }
            return Promise.reject(null);
        });
    }
}
exports.displayTestFrameworkError = displayTestFrameworkError;
function displayPromptToEnableTests(rootDir, outputChannel, installer) {
    return __awaiter(this, void 0, void 0, function* () {
        const settings = configSettings_1.PythonSettings.getInstance(vscode.Uri.file(rootDir));
        if (settings.unitTest.pyTestEnabled ||
            settings.unitTest.nosetestsEnabled ||
            settings.unitTest.unittestEnabled) {
            return;
        }
        if (!settings.unitTest.promptToConfigure) {
            return;
        }
        const yes = 'Yes';
        const no = 'Later';
        const noNotAgain = 'No, don\'t ask again';
        const hasTests = checkForExistenceOfTests(rootDir);
        if (!hasTests) {
            return;
        }
        const item = yield vscode.window.showInformationMessage('You seem to have tests, would you like to enable a test framework?', yes, no, noNotAgain);
        if (!item || item === no) {
            return;
        }
        if (item === yes) {
            yield promptToEnableAndConfigureTestFramework(vscode.workspace.getWorkspaceFolder(vscode.Uri.file(rootDir)).uri, installer, outputChannel);
        }
        else {
            const pythonConfig = vscode.workspace.getConfiguration('python');
            yield pythonConfig.update('unitTest.promptToConfigure', false);
        }
    });
}
exports.displayPromptToEnableTests = displayPromptToEnableTests;
// Configure everything before enabling.
// Cuz we don't want the test engine (in main.ts file - tests get discovered when config changes are detected)
// to start discovering tests when tests haven't been configured properly.
function enableTest(wkspace, configMgr) {
    const pythonConfig = vscode.workspace.getConfiguration('python', wkspace);
    // tslint:disable-next-line:no-backbone-get-set-outside-model
    if (pythonConfig.get('unitTest.promptToConfigure')) {
        return configMgr.enable();
    }
    return pythonConfig.update('unitTest.promptToConfigure', undefined).then(() => {
        return configMgr.enable();
    }, reason => {
        return configMgr.enable().then(() => Promise.reject(reason));
    });
}
function checkForExistenceOfTests(rootDir) {
    return utils_1.getSubDirectories(rootDir).then(subDirs => {
        return subDirs.map(dir => path.relative(rootDir, dir)).filter(dir => dir.match(/test/i)).length > 0;
    });
}
function createTestConfigurationManager(wkspace, product, outputChannel, installer) {
    const configSettingService = new configSettingService_1.TestConfigSettingsService();
    switch (product) {
        case types_1.Product.unittest: {
            return new unittest.ConfigurationManager(wkspace, outputChannel, installer, configSettingService);
        }
        case types_1.Product.pytest: {
            return new pytest.ConfigurationManager(wkspace, outputChannel, installer, configSettingService);
        }
        case types_1.Product.nosetest: {
            return new nose.ConfigurationManager(wkspace, outputChannel, installer, configSettingService);
        }
        default: {
            throw new Error('Invalid test configuration');
        }
    }
}
function selectTestRunner(placeHolderMessage) {
    return __awaiter(this, void 0, void 0, function* () {
        const items = [{
                label: 'unittest',
                product: types_1.Product.unittest,
                description: 'Standard Python test framework',
                detail: 'https://docs.python.org/3/library/unittest.html'
            },
            {
                label: 'pytest',
                product: types_1.Product.pytest,
                description: 'Can run unittest (including trial) and nose test suites out of the box',
                // tslint:disable-next-line:no-http-string
                detail: 'http://docs.pytest.org/'
            },
            {
                label: 'nose',
                product: types_1.Product.nosetest,
                description: 'nose framework',
                detail: 'https://nose.readthedocs.io/'
            }];
        const options = {
            matchOnDescription: true,
            matchOnDetail: true,
            placeHolder: placeHolderMessage
        };
        const selectedTestRunner = yield vscode.window.showQuickPick(items, options);
        // tslint:disable-next-line:prefer-type-cast
        return selectedTestRunner ? selectedTestRunner.product : undefined;
    });
}
//# sourceMappingURL=configuration.js.map