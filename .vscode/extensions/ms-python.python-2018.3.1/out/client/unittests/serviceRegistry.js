"use strict";
// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT License.
Object.defineProperty(exports, "__esModule", { value: true });
const types_1 = require("../ioc/types");
const constants_1 = require("./common/constants");
const debugLauncher_1 = require("./common/debugLauncher");
const storageService_1 = require("./common/services/storageService");
const testManagerService_1 = require("./common/services/testManagerService");
const testResultsService_1 = require("./common/services/testResultsService");
const workspaceTestManagerService_1 = require("./common/services/workspaceTestManagerService");
const testUtils_1 = require("./common/testUtils");
const flatteningVisitor_1 = require("./common/testVisitors/flatteningVisitor");
const folderGenerationVisitor_1 = require("./common/testVisitors/folderGenerationVisitor");
const resultResetVisitor_1 = require("./common/testVisitors/resultResetVisitor");
const types_2 = require("./common/types");
const types_3 = require("./common/types");
const main_1 = require("./nosetest/main");
const discoveryService_1 = require("./nosetest/services/discoveryService");
const parserService_1 = require("./nosetest/services/parserService");
const main_2 = require("./pytest/main");
const discoveryService_2 = require("./pytest/services/discoveryService");
const parserService_2 = require("./pytest/services/parserService");
const main_3 = require("./unittest/main");
const discoveryService_3 = require("./unittest/services/discoveryService");
const parserService_3 = require("./unittest/services/parserService");
const socketServer_1 = require("./unittest/socketServer");
function registerTypes(serviceManager) {
    serviceManager.addSingleton(types_2.ITestDebugLauncher, debugLauncher_1.DebugLauncher);
    serviceManager.addSingleton(types_2.ITestCollectionStorageService, storageService_1.TestCollectionStorageService);
    serviceManager.addSingleton(types_3.IWorkspaceTestManagerService, workspaceTestManagerService_1.WorkspaceTestManagerService);
    serviceManager.add(types_3.ITestsHelper, testUtils_1.TestsHelper);
    serviceManager.add(types_2.IUnitTestSocketServer, socketServer_1.UnitTestSocketServer);
    serviceManager.add(types_3.ITestResultsService, testResultsService_1.TestResultsService);
    serviceManager.add(types_3.ITestVisitor, flatteningVisitor_1.TestFlatteningVisitor, 'TestFlatteningVisitor');
    serviceManager.add(types_3.ITestVisitor, folderGenerationVisitor_1.TestFolderGenerationVisitor, 'TestFolderGenerationVisitor');
    serviceManager.add(types_3.ITestVisitor, resultResetVisitor_1.TestResultResetVisitor, 'TestResultResetVisitor');
    serviceManager.add(types_3.ITestsParser, parserService_3.TestsParser, constants_1.UNITTEST_PROVIDER);
    serviceManager.add(types_3.ITestsParser, parserService_2.TestsParser, constants_1.PYTEST_PROVIDER);
    serviceManager.add(types_3.ITestsParser, parserService_1.TestsParser, constants_1.NOSETEST_PROVIDER);
    serviceManager.add(types_2.ITestDiscoveryService, discoveryService_3.TestDiscoveryService, constants_1.UNITTEST_PROVIDER);
    serviceManager.add(types_2.ITestDiscoveryService, discoveryService_2.TestDiscoveryService, constants_1.PYTEST_PROVIDER);
    serviceManager.add(types_2.ITestDiscoveryService, discoveryService_1.TestDiscoveryService, constants_1.NOSETEST_PROVIDER);
    serviceManager.addFactory(types_2.ITestManagerFactory, (context) => {
        return (testProvider, workspaceFolder, rootDirectory) => {
            const serviceContainer = context.container.get(types_1.IServiceContainer);
            switch (testProvider) {
                case constants_1.NOSETEST_PROVIDER: {
                    return new main_1.TestManager(workspaceFolder, rootDirectory, serviceContainer);
                }
                case constants_1.PYTEST_PROVIDER: {
                    return new main_2.TestManager(workspaceFolder, rootDirectory, serviceContainer);
                }
                case constants_1.UNITTEST_PROVIDER: {
                    return new main_3.TestManager(workspaceFolder, rootDirectory, serviceContainer);
                }
                default: {
                    throw new Error(`Unrecognized test provider '${testProvider}'`);
                }
            }
        };
    });
    serviceManager.addFactory(types_2.ITestManagerServiceFactory, (context) => {
        return (workspaceFolder) => {
            const serviceContainer = context.container.get(types_1.IServiceContainer);
            const testsHelper = context.container.get(types_3.ITestsHelper);
            return new testManagerService_1.TestManagerService(workspaceFolder, testsHelper, serviceContainer);
        };
    });
}
exports.registerTypes = registerTypes;
//# sourceMappingURL=serviceRegistry.js.map