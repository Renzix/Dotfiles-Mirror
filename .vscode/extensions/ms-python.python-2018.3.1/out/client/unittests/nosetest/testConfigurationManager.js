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
const fs = require("fs");
const path = require("path");
const types_1 = require("../../common/types");
const testConfigurationManager_1 = require("../common/managers/testConfigurationManager");
class ConfigurationManager extends testConfigurationManager_1.TestConfigurationManager {
    constructor(workspace, outputChannel, installer, testConfigSettingsService) {
        super(workspace, types_1.Product.nosetest, outputChannel, installer, testConfigSettingsService);
    }
    static configFilesExist(rootDir) {
        return __awaiter(this, void 0, void 0, function* () {
            const promises = ['.noserc', 'nose.cfg'].map(cfg => {
                return new Promise(resolve => {
                    fs.exists(path.join(rootDir, cfg), exists => { resolve(exists ? cfg : ''); });
                });
            });
            const values = yield Promise.all(promises);
            return values.filter(exists => exists.length > 0);
        });
    }
    // tslint:disable-next-line:no-any
    configure(wkspace) {
        return __awaiter(this, void 0, void 0, function* () {
            const args = [];
            const configFileOptionLabel = 'Use existing config file';
            const configFiles = yield ConfigurationManager.configFilesExist(wkspace.fsPath);
            // If a config file exits, there's nothing to be configured.
            if (configFiles.length > 0) {
                return;
            }
            const subDirs = yield this.getTestDirs(wkspace.fsPath);
            const testDir = yield this.selectTestDir(wkspace.fsPath, subDirs);
            if (typeof testDir === 'string' && testDir !== configFileOptionLabel) {
                args.push(testDir);
            }
            const installed = yield this.installer.isInstalled(types_1.Product.nosetest);
            if (!installed) {
                yield this.installer.install(types_1.Product.nosetest);
            }
            yield this.testConfigSettingsService.updateTestArgs(wkspace.fsPath, types_1.Product.nosetest, args);
        });
    }
}
exports.ConfigurationManager = ConfigurationManager;
//# sourceMappingURL=testConfigurationManager.js.map