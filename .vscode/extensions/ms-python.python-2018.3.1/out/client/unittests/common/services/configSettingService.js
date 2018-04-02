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
const vscode_1 = require("vscode");
const types_1 = require("../../../common/types");
class TestConfigSettingsService {
    static getTestArgSetting(product) {
        switch (product) {
            case types_1.Product.unittest:
                return 'unitTest.unittestArgs';
            case types_1.Product.pytest:
                return 'unitTest.pyTestArgs';
            case types_1.Product.nosetest:
                return 'unitTest.nosetestArgs';
            default:
                throw new Error('Invalid Test Product');
        }
    }
    static getTestEnablingSetting(product) {
        switch (product) {
            case types_1.Product.unittest:
                return 'unitTest.unittestEnabled';
            case types_1.Product.pytest:
                return 'unitTest.pyTestEnabled';
            case types_1.Product.nosetest:
                return 'unitTest.nosetestsEnabled';
            default:
                throw new Error('Invalid Test Product');
        }
    }
    // tslint:disable-next-line:no-any
    static updateSetting(testDirectory, setting, value) {
        return __awaiter(this, void 0, void 0, function* () {
            let pythonConfig;
            const resource = typeof testDirectory === 'string' ? vscode_1.Uri.file(testDirectory) : testDirectory;
            if (!Array.isArray(vscode_1.workspace.workspaceFolders) || vscode_1.workspace.workspaceFolders.length === 0) {
                pythonConfig = vscode_1.workspace.getConfiguration('python');
            }
            else if (vscode_1.workspace.workspaceFolders.length === 1) {
                pythonConfig = vscode_1.workspace.getConfiguration('python', vscode_1.workspace.workspaceFolders[0].uri);
            }
            else {
                const workspaceFolder = vscode_1.workspace.getWorkspaceFolder(resource);
                if (!workspaceFolder) {
                    throw new Error(`Test directory does not belong to any workspace (${testDirectory})`);
                }
                // tslint:disable-next-line:no-non-null-assertion
                pythonConfig = vscode_1.workspace.getConfiguration('python', workspaceFolder.uri);
            }
            return pythonConfig.update(setting, value);
        });
    }
    updateTestArgs(testDirectory, product, args) {
        return __awaiter(this, void 0, void 0, function* () {
            const setting = TestConfigSettingsService.getTestArgSetting(product);
            return TestConfigSettingsService.updateSetting(testDirectory, setting, args);
        });
    }
    enable(testDirectory, product) {
        return __awaiter(this, void 0, void 0, function* () {
            const setting = TestConfigSettingsService.getTestEnablingSetting(product);
            return TestConfigSettingsService.updateSetting(testDirectory, setting, true);
        });
    }
    disable(testDirectory, product) {
        return __awaiter(this, void 0, void 0, function* () {
            const setting = TestConfigSettingsService.getTestEnablingSetting(product);
            return TestConfigSettingsService.updateSetting(testDirectory, setting, false);
        });
    }
}
exports.TestConfigSettingsService = TestConfigSettingsService;
//# sourceMappingURL=configSettingService.js.map