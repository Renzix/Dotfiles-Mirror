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
const vscode = require("vscode");
// tslint:disable-next-line:no-duplicate-imports
const vscode_1 = require("vscode");
const configSettings_1 = require("../common/configSettings");
const constants = require("../common/constants");
const types_1 = require("../common/types");
const constants_1 = require("../telemetry/constants");
const index_1 = require("../telemetry/index");
const main_1 = require("./codeLenses/main");
const constants_2 = require("./common/constants");
const testUtils_1 = require("./common/testUtils");
const types_2 = require("./common/types");
const configuration_1 = require("./configuration");
const main_2 = require("./display/main");
const picker_1 = require("./display/picker");
let workspaceTestManagerService;
let testResultDisplay;
let testDisplay;
let outChannel;
const onDidChange = new vscode.EventEmitter();
let testCollectionStorage;
let _serviceContaner;
function activate(context, outputChannel, symboldProvider, serviceContainer) {
    _serviceContaner = serviceContainer;
    context.subscriptions.push({ dispose: dispose });
    outChannel = outputChannel;
    const disposables = registerCommands();
    context.subscriptions.push(...disposables);
    testCollectionStorage = serviceContainer.get(types_2.ITestCollectionStorageService);
    workspaceTestManagerService = serviceContainer.get(types_2.IWorkspaceTestManagerService);
    context.subscriptions.push(autoResetTests());
    context.subscriptions.push(main_1.activateCodeLenses(onDidChange, symboldProvider, testCollectionStorage));
    context.subscriptions.push(vscode.workspace.onDidSaveTextDocument(onDocumentSaved));
    autoDiscoverTests();
}
exports.activate = activate;
function getTestManager(displayTestNotConfiguredMessage, resource) {
    return __awaiter(this, void 0, void 0, function* () {
        let wkspace;
        if (resource) {
            const wkspaceFolder = vscode_1.workspace.getWorkspaceFolder(resource);
            wkspace = wkspaceFolder ? wkspaceFolder.uri : undefined;
        }
        else {
            wkspace = yield testUtils_1.selectTestWorkspace();
        }
        if (!wkspace) {
            return;
        }
        const testManager = workspaceTestManagerService.getTestManager(wkspace);
        if (testManager) {
            return testManager;
        }
        if (displayTestNotConfiguredMessage) {
            yield configuration_1.displayTestFrameworkError(wkspace, outChannel, _serviceContaner.get(types_1.IInstaller));
        }
    });
}
let timeoutId;
function onDocumentSaved(doc) {
    return __awaiter(this, void 0, void 0, function* () {
        const testManager = yield getTestManager(false, doc.uri);
        if (!testManager) {
            return;
        }
        const tests = yield testManager.discoverTests(constants_2.CommandSource.auto, false, true);
        if (!tests || !Array.isArray(tests.testFiles) || tests.testFiles.length === 0) {
            return;
        }
        if (tests.testFiles.findIndex((f) => f.fullPath === doc.uri.fsPath) === -1) {
            return;
        }
        if (timeoutId) {
            clearTimeout(timeoutId);
        }
        timeoutId = setTimeout(() => discoverTests(constants_2.CommandSource.auto, doc.uri, true, false, true), 1000);
    });
}
function dispose() {
    workspaceTestManagerService.dispose();
    testCollectionStorage.dispose();
}
function registerCommands() {
    const disposables = [];
    disposables.push(vscode.commands.registerCommand(constants.Commands.Tests_Discover, (_, cmdSource = constants_2.CommandSource.commandPalette, resource) => {
        // Ignore the exceptions returned.
        // This command will be invoked else where in the extension.
        // tslint:disable-next-line:no-empty
        discoverTests(cmdSource, resource, true, true).catch(() => { });
    }));
    disposables.push(vscode.commands.registerCommand(constants.Commands.Tests_Run_Failed, (_, cmdSource = constants_2.CommandSource.commandPalette, resource) => runTestsImpl(cmdSource, resource, undefined, true)));
    // tslint:disable-next-line:no-unnecessary-callback-wrapper
    disposables.push(vscode.commands.registerCommand(constants.Commands.Tests_Run, (_, cmdSource = constants_2.CommandSource.commandPalette, file, testToRun) => runTestsImpl(cmdSource, file, testToRun)));
    disposables.push(vscode.commands.registerCommand(constants.Commands.Tests_Debug, (_, cmdSource = constants_2.CommandSource.commandPalette, file, testToRun) => runTestsImpl(cmdSource, file, testToRun, false, true)));
    // tslint:disable-next-line:no-unnecessary-callback-wrapper
    disposables.push(vscode.commands.registerCommand(constants.Commands.Tests_View_UI, () => displayUI(constants_2.CommandSource.commandPalette)));
    // tslint:disable-next-line:no-unnecessary-callback-wrapper
    disposables.push(vscode.commands.registerCommand(constants.Commands.Tests_Picker_UI, (_, cmdSource = constants_2.CommandSource.commandPalette, file, testFunctions) => displayPickerUI(cmdSource, file, testFunctions)));
    disposables.push(vscode.commands.registerCommand(constants.Commands.Tests_Picker_UI_Debug, (_, cmdSource = constants_2.CommandSource.commandPalette, file, testFunctions) => displayPickerUI(cmdSource, file, testFunctions, true)));
    // tslint:disable-next-line:no-unnecessary-callback-wrapper
    disposables.push(vscode.commands.registerCommand(constants.Commands.Tests_Stop, (_, resource) => stopTests(resource)));
    // tslint:disable-next-line:no-unnecessary-callback-wrapper
    disposables.push(vscode.commands.registerCommand(constants.Commands.Tests_ViewOutput, (_, cmdSource = constants_2.CommandSource.commandPalette) => viewOutput(cmdSource)));
    disposables.push(vscode.commands.registerCommand(constants.Commands.Tests_Ask_To_Stop_Discovery, () => displayStopUI('Stop discovering tests')));
    disposables.push(vscode.commands.registerCommand(constants.Commands.Tests_Ask_To_Stop_Test, () => displayStopUI('Stop running tests')));
    // tslint:disable-next-line:no-unnecessary-callback-wrapper
    disposables.push(vscode.commands.registerCommand(constants.Commands.Tests_Select_And_Run_Method, (_, cmdSource = constants_2.CommandSource.commandPalette, resource) => selectAndRunTestMethod(cmdSource, resource)));
    disposables.push(vscode.commands.registerCommand(constants.Commands.Tests_Select_And_Debug_Method, (_, cmdSource = constants_2.CommandSource.commandPalette, resource) => selectAndRunTestMethod(cmdSource, resource, true)));
    // tslint:disable-next-line:no-unnecessary-callback-wrapper
    disposables.push(vscode.commands.registerCommand(constants.Commands.Tests_Select_And_Run_File, (_, cmdSource = constants_2.CommandSource.commandPalette) => selectAndRunTestFile(cmdSource)));
    // tslint:disable-next-line:no-unnecessary-callback-wrapper
    disposables.push(vscode.commands.registerCommand(constants.Commands.Tests_Run_Current_File, (_, cmdSource = constants_2.CommandSource.commandPalette) => runCurrentTestFile(cmdSource)));
    return disposables;
}
function viewOutput(cmdSource) {
    index_1.sendTelemetryEvent(constants_1.UNITTEST_VIEW_OUTPUT);
    outChannel.show();
}
function displayUI(cmdSource) {
    return __awaiter(this, void 0, void 0, function* () {
        const testManager = yield getTestManager(true);
        if (!testManager) {
            return;
        }
        testDisplay = testDisplay ? testDisplay : new picker_1.TestDisplay(testCollectionStorage);
        testDisplay.displayTestUI(cmdSource, testManager.workspaceFolder);
    });
}
function displayPickerUI(cmdSource, file, testFunctions, debug) {
    return __awaiter(this, void 0, void 0, function* () {
        const testManager = yield getTestManager(true, file);
        if (!testManager) {
            return;
        }
        testDisplay = testDisplay ? testDisplay : new picker_1.TestDisplay(testCollectionStorage);
        testDisplay.displayFunctionTestPickerUI(cmdSource, testManager.workspaceFolder, testManager.workingDirectory, file, testFunctions, debug);
    });
}
function selectAndRunTestMethod(cmdSource, resource, debug) {
    return __awaiter(this, void 0, void 0, function* () {
        const testManager = yield getTestManager(true, resource);
        if (!testManager) {
            return;
        }
        try {
            yield testManager.discoverTests(cmdSource, true, true, true);
        }
        catch (ex) {
            return;
        }
        const tests = testCollectionStorage.getTests(testManager.workspaceFolder);
        testDisplay = testDisplay ? testDisplay : new picker_1.TestDisplay(testCollectionStorage);
        const selectedTestFn = yield testDisplay.selectTestFunction(testManager.workspaceFolder.fsPath, tests);
        if (!selectedTestFn) {
            return;
        }
        // tslint:disable-next-line:prefer-type-cast no-object-literal-type-assertion
        yield runTestsImpl(cmdSource, testManager.workspaceFolder, { testFunction: [selectedTestFn.testFunction] }, false, debug);
    });
}
function selectAndRunTestFile(cmdSource) {
    return __awaiter(this, void 0, void 0, function* () {
        const testManager = yield getTestManager(true);
        if (!testManager) {
            return;
        }
        try {
            yield testManager.discoverTests(cmdSource, true, true, true);
        }
        catch (ex) {
            return;
        }
        const tests = testCollectionStorage.getTests(testManager.workspaceFolder);
        testDisplay = testDisplay ? testDisplay : new picker_1.TestDisplay(testCollectionStorage);
        const selectedFile = yield testDisplay.selectTestFile(testManager.workspaceFolder.fsPath, tests);
        if (!selectedFile) {
            return;
        }
        // tslint:disable-next-line:prefer-type-cast no-object-literal-type-assertion
        yield runTestsImpl(cmdSource, testManager.workspaceFolder, { testFile: [selectedFile] });
    });
}
function runCurrentTestFile(cmdSource) {
    return __awaiter(this, void 0, void 0, function* () {
        if (!vscode_1.window.activeTextEditor) {
            return;
        }
        const testManager = yield getTestManager(true, vscode_1.window.activeTextEditor.document.uri);
        if (!testManager) {
            return;
        }
        try {
            yield testManager.discoverTests(cmdSource, true, true, true);
        }
        catch (ex) {
            return;
        }
        const tests = testCollectionStorage.getTests(testManager.workspaceFolder);
        const testFiles = tests.testFiles.filter(testFile => {
            return testFile.fullPath === vscode_1.window.activeTextEditor.document.uri.fsPath;
        });
        if (testFiles.length < 1) {
            return;
        }
        // tslint:disable-next-line:prefer-type-cast no-object-literal-type-assertion
        yield runTestsImpl(cmdSource, testManager.workspaceFolder, { testFile: [testFiles[0]] });
    });
}
function displayStopUI(message) {
    return __awaiter(this, void 0, void 0, function* () {
        const testManager = yield getTestManager(true);
        if (!testManager) {
            return;
        }
        testDisplay = testDisplay ? testDisplay : new picker_1.TestDisplay(testCollectionStorage);
        testDisplay.displayStopTestUI(testManager.workspaceFolder, message);
    });
}
let uniTestSettingsString;
function autoResetTests() {
    if (!Array.isArray(vscode_1.workspace.workspaceFolders) || vscode_1.workspace.workspaceFolders.length > 1) {
        // tslint:disable-next-line:no-empty
        return { dispose: () => { } };
    }
    const settings = configSettings_1.PythonSettings.getInstance();
    uniTestSettingsString = JSON.stringify(settings.unitTest);
    return vscode_1.workspace.onDidChangeConfiguration(() => setTimeout(onConfigChanged, 1000));
}
function onConfigChanged() {
    // If there's one workspace, then stop the tests and restart,
    // else let the user do this manually.
    if (!Array.isArray(vscode_1.workspace.workspaceFolders) || vscode_1.workspace.workspaceFolders.length > 1) {
        return;
    }
    const settings = configSettings_1.PythonSettings.getInstance();
    // Possible that a test framework has been enabled or some settings have changed.
    // Meaning we need to re-load the discovered tests (as something could have changed).
    const newSettings = JSON.stringify(settings.unitTest);
    if (uniTestSettingsString === newSettings) {
        return;
    }
    uniTestSettingsString = newSettings;
    if (!settings.unitTest.nosetestsEnabled && !settings.unitTest.pyTestEnabled && !settings.unitTest.unittestEnabled) {
        if (testResultDisplay) {
            testResultDisplay.enabled = false;
        }
        workspaceTestManagerService.dispose();
        return;
    }
    if (testResultDisplay) {
        testResultDisplay.enabled = true;
    }
    autoDiscoverTests();
}
function autoDiscoverTests() {
    if (!Array.isArray(vscode_1.workspace.workspaceFolders) || vscode_1.workspace.workspaceFolders.length > 1) {
        return;
    }
    const settings = configSettings_1.PythonSettings.getInstance();
    if (!settings.unitTest.nosetestsEnabled && !settings.unitTest.pyTestEnabled && !settings.unitTest.unittestEnabled) {
        return;
    }
    // No need to display errors.
    // tslint:disable-next-line:no-empty
    discoverTests(constants_2.CommandSource.auto, vscode_1.workspace.workspaceFolders[0].uri, true).catch(() => { });
}
function stopTests(resource) {
    return __awaiter(this, void 0, void 0, function* () {
        index_1.sendTelemetryEvent(constants_1.UNITTEST_STOP);
        const testManager = yield getTestManager(true, resource);
        if (testManager) {
            testManager.stop();
        }
    });
}
function discoverTests(cmdSource, resource, ignoreCache, userInitiated, quietMode) {
    return __awaiter(this, void 0, void 0, function* () {
        const testManager = yield getTestManager(true, resource);
        if (!testManager) {
            return;
        }
        if (testManager && (testManager.status !== types_2.TestStatus.Discovering && testManager.status !== types_2.TestStatus.Running)) {
            testResultDisplay = testResultDisplay ? testResultDisplay : new main_2.TestResultDisplay(onDidChange);
            const discoveryPromise = testManager.discoverTests(cmdSource, ignoreCache, quietMode, userInitiated);
            testResultDisplay.displayDiscoverStatus(discoveryPromise, quietMode)
                .catch(ex => console.error('Python Extension: displayDiscoverStatus', ex));
            yield discoveryPromise;
        }
    });
}
function runTestsImpl(cmdSource, resource, testsToRun, runFailedTests, debug = false) {
    return __awaiter(this, void 0, void 0, function* () {
        const testManager = yield getTestManager(true, resource);
        if (!testManager) {
            return;
        }
        testResultDisplay = testResultDisplay ? testResultDisplay : new main_2.TestResultDisplay(onDidChange);
        const promise = testManager.runTest(cmdSource, testsToRun, runFailedTests, debug)
            .catch(reason => {
            if (reason !== constants_2.CANCELLATION_REASON) {
                outChannel.appendLine(`Error: ${reason}`);
            }
            return Promise.reject(reason);
        });
        testResultDisplay.displayProgressStatus(promise, debug);
        yield promise;
    });
}
//# sourceMappingURL=main.js.map