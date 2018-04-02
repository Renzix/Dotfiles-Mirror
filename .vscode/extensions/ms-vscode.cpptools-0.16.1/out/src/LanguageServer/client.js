'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const path = require("path");
const vscode = require("vscode");
const vscode_languageclient_1 = require("vscode-languageclient");
const util = require("../common");
const configs = require("./configurations");
const settings_1 = require("./settings");
const telemetry = require("../telemetry");
const persistentState_1 = require("./persistentState");
const ui_1 = require("./ui");
const protocolFilter_1 = require("./protocolFilter");
const dataBinding_1 = require("./dataBinding");
const minimatch = require("minimatch");
const logger = require("../logger");
const extension_1 = require("./extension");
let ui;
const NavigationListRequest = new vscode_languageclient_1.RequestType('cpptools/requestNavigationList');
const GoToDeclarationRequest = new vscode_languageclient_1.RequestType('cpptools/goToDeclaration');
const QueryCompilerDefaultsRequest = new vscode_languageclient_1.RequestType('cpptools/queryCompilerDefaults');
const SwitchHeaderSourceRequest = new vscode_languageclient_1.RequestType('cpptools/didSwitchHeaderSource');
const DidOpenNotification = new vscode_languageclient_1.NotificationType('textDocument/didOpen');
const FileCreatedNotification = new vscode_languageclient_1.NotificationType('cpptools/fileCreated');
const FileDeletedNotification = new vscode_languageclient_1.NotificationType('cpptools/fileDeleted');
const ResetDatabaseNotification = new vscode_languageclient_1.NotificationType('cpptools/resetDatabase');
const PauseParsingNotification = new vscode_languageclient_1.NotificationType('cpptools/pauseParsing');
const ResumeParsingNotification = new vscode_languageclient_1.NotificationType('cpptools/resumeParsing');
const ActiveDocumentChangeNotification = new vscode_languageclient_1.NotificationType('cpptools/activeDocumentChange');
const TextEditorSelectionChangeNotification = new vscode_languageclient_1.NotificationType('cpptools/textEditorSelectionChange');
const ChangeFolderSettingsNotification = new vscode_languageclient_1.NotificationType('cpptools/didChangeFolderSettings');
const ChangeCompileCommandsNotification = new vscode_languageclient_1.NotificationType('cpptools/didChangeCompileCommands');
const ChangeSelectedSettingNotification = new vscode_languageclient_1.NotificationType('cpptools/didChangeSelectedSetting');
const IntervalTimerNotification = new vscode_languageclient_1.NotificationType('cpptools/onIntervalTimer');
const ReloadWindowNotification = new vscode_languageclient_1.NotificationType('cpptools/reloadWindow');
const LogTelemetryNotification = new vscode_languageclient_1.NotificationType('cpptools/logTelemetry');
const ReportNavigationNotification = new vscode_languageclient_1.NotificationType('cpptools/reportNavigation');
const ReportTagParseStatusNotification = new vscode_languageclient_1.NotificationType('cpptools/reportTagParseStatus');
const ReportStatusNotification = new vscode_languageclient_1.NotificationType('cpptools/reportStatus');
const DebugProtocolNotification = new vscode_languageclient_1.NotificationType('cpptools/debugProtocol');
const DebugLogNotification = new vscode_languageclient_1.NotificationType('cpptools/debugLog');
const InactiveRegionNotification = new vscode_languageclient_1.NotificationType('cpptools/inactiveRegions');
const maxSettingLengthForTelemetry = 50;
let previousCppSettings = {};
function collectSettingsForTelemetry(filter, resource) {
    let settings = vscode.workspace.getConfiguration("C_Cpp", resource);
    let result = {};
    for (let key in settings) {
        if (settings.inspect(key).defaultValue === undefined) {
            continue;
        }
        let val = settings.get(key);
        if (val instanceof Object) {
            val = JSON.stringify(val, null, 2);
        }
        let curSetting = util.packageJson.contributes.configuration.properties["C_Cpp." + key];
        if (curSetting) {
            let curEnum = curSetting["enum"];
            if (curEnum && curEnum.indexOf(val) === -1) {
                continue;
            }
        }
        if (filter(key, val, settings)) {
            previousCppSettings[key] = val;
            switch (String(key).toLowerCase()) {
                case "clang_format_path": {
                    continue;
                }
                case "clang_format_style":
                case "clang_format_fallbackstyle": {
                    let newKey = String(key) + "2";
                    if (val) {
                        switch (String(val).toLowerCase()) {
                            case "visual studio":
                            case "llvm":
                            case "google":
                            case "chromium":
                            case "mozilla":
                            case "webkit":
                            case "file":
                            case "none": {
                                result[newKey] = String(previousCppSettings[key]);
                                break;
                            }
                            default: {
                                result[newKey] = "...";
                                break;
                            }
                        }
                    }
                    else {
                        result[newKey] = "null";
                    }
                    key = newKey;
                    break;
                }
                default: {
                    result[key] = String(previousCppSettings[key]);
                    break;
                }
            }
            if (result[key].length > maxSettingLengthForTelemetry) {
                result[key] = result[key].substr(0, maxSettingLengthForTelemetry) + "...";
            }
        }
    }
    return result;
}
function initializeSettingsCache(resource) {
    collectSettingsForTelemetry(() => true, resource);
}
function getNonDefaultSettings(resource) {
    let filter = (key, val, settings) => {
        return val !== settings.inspect(key).defaultValue;
    };
    initializeSettingsCache(resource);
    return collectSettingsForTelemetry(filter, resource);
}
function createClient(allClients, workspaceFolder) {
    return new DefaultClient(allClients, workspaceFolder);
}
exports.createClient = createClient;
function createNullClient() {
    return new NullClient();
}
exports.createNullClient = createNullClient;
class DefaultClient {
    constructor(allClients, workspaceFolder) {
        this.disposables = [];
        this.trackedDocuments = new Set();
        this.crashTimes = [];
        this.failureMessageShown = new persistentState_1.PersistentState("DefaultClient.failureMessageShown", false);
        this.isSupported = true;
        this.inactiveRegionsDecorations = new Map();
        this.model = {
            isTagParsing: new dataBinding_1.DataBinding(false),
            isUpdatingIntelliSense: new dataBinding_1.DataBinding(false),
            navigationLocation: new dataBinding_1.DataBinding(""),
            tagParserStatus: new dataBinding_1.DataBinding(""),
            activeConfigName: new dataBinding_1.DataBinding("")
        };
        try {
            let languageClient = this.createLanguageClient(allClients, workspaceFolder);
            languageClient.registerProposedFeatures();
            languageClient.start();
            util.setProgress(util.getProgressExecutableStarted());
            this.rootFolder = workspaceFolder;
            ui = ui_1.getUI();
            ui.bind(this);
            this.onReadyPromise = languageClient.onReady().then(() => {
                this.configuration = new configs.CppProperties(this.RootPath);
                this.configuration.ConfigurationsChanged((e) => this.onConfigurationsChanged(e));
                this.configuration.SelectionChanged((e) => this.onSelectedConfigurationChanged(e));
                this.configuration.CompileCommandsChanged((e) => this.onCompileCommandsChanged(e));
                this.disposables.push(this.configuration);
                languageClient.sendRequest(QueryCompilerDefaultsRequest, {}).then((compilerDefaults) => {
                    this.configuration.CompilerDefaults = compilerDefaults;
                });
                this.languageClient = languageClient;
                telemetry.logLanguageServerEvent("NonDefaultInitialCppSettings", getNonDefaultSettings(this.RootUri));
                this.failureMessageShown.Value = false;
                this.registerNotifications();
                this.registerFileWatcher();
            }, () => {
                this.isSupported = false;
                if (!this.failureMessageShown.Value) {
                    this.failureMessageShown.Value = true;
                    vscode.window.showErrorMessage("Unable to start the C/C++ language server. IntelliSense features will be disabled.");
                }
            });
        }
        catch (_a) {
            this.isSupported = false;
            if (!this.failureMessageShown.Value) {
                this.failureMessageShown.Value = true;
                vscode.window.showErrorMessage("Unable to start the C/C++ language server. IntelliSense features will be disabled.");
            }
        }
    }
    get TagParsingChanged() { return this.model.isTagParsing.ValueChanged; }
    get IntelliSenseParsingChanged() { return this.model.isUpdatingIntelliSense.ValueChanged; }
    get NavigationLocationChanged() { return this.model.navigationLocation.ValueChanged; }
    get TagParserStatusChanged() { return this.model.tagParserStatus.ValueChanged; }
    get ActiveConfigChanged() { return this.model.activeConfigName.ValueChanged; }
    get RootPath() {
        return (this.rootFolder) ? this.rootFolder.uri.fsPath : "";
    }
    get RootUri() {
        return (this.rootFolder) ? this.rootFolder.uri : null;
    }
    get Name() {
        return this.getName(this.rootFolder);
    }
    get TrackedDocuments() {
        return this.trackedDocuments;
    }
    getName(workspaceFolder) {
        return workspaceFolder ? workspaceFolder.name : "untitled";
    }
    createLanguageClient(allClients, workspaceFolder) {
        let serverModule = getLanguageServerFileName();
        let serverName = this.getName(workspaceFolder);
        let serverOptions = {
            run: { command: serverModule },
            debug: { command: serverModule, args: [serverName] }
        };
        let settings = new settings_1.CppSettings(workspaceFolder ? workspaceFolder.uri : null);
        let other = new settings_1.OtherSettings(workspaceFolder ? workspaceFolder.uri : null);
        let storagePath = util.extensionContext.storagePath;
        if (vscode.workspace.workspaceFolders && vscode.workspace.workspaceFolders.length > 1) {
            storagePath = path.join(storagePath, serverName);
        }
        let clientOptions = {
            documentSelector: [
                { scheme: 'file', language: 'cpp' },
                { scheme: 'file', language: 'c' }
            ],
            synchronize: {
                configurationSection: ['C_Cpp', 'files', 'search']
            },
            workspaceFolder: workspaceFolder,
            initializationOptions: {
                clang_format_path: settings.clangFormatPath,
                clang_format_style: settings.clangFormatStyle,
                clang_format_fallbackStyle: settings.clangFormatFallbackStyle,
                clang_format_sortIncludes: settings.clangFormatSortIncludes,
                formatting: settings.formatting,
                extension_path: util.extensionContext.extensionPath,
                exclude_files: other.filesExclude,
                exclude_search: other.searchExclude,
                storage_path: storagePath,
                tab_size: other.editorTabSize,
                intelliSenseEngine: settings.intelliSenseEngine,
                intelliSenseEngineFallback: settings.intelliSenseEngineFallback,
                autocomplete: settings.autoComplete,
                errorSquiggles: settings.errorSquiggles,
                dimInactiveRegions: settings.dimInactiveRegions,
                loggingLevel: settings.loggingLevel,
                workspaceParsingPriority: settings.workspaceParsingPriority,
                exclusionPolicy: settings.exclusionPolicy
            },
            middleware: protocolFilter_1.createProtocolFilter(this, allClients),
            errorHandler: {
                error: () => vscode_languageclient_1.ErrorAction.Continue,
                closed: () => {
                    this.crashTimes.push(Date.now());
                    if (this.crashTimes.length < 5) {
                        let newClient = allClients.replace(this, true);
                        newClient.crashTimes = this.crashTimes;
                    }
                    else {
                        let elapsed = this.crashTimes[this.crashTimes.length - 1] - this.crashTimes[0];
                        if (elapsed <= 3 * 60 * 1000) {
                            if (vscode.workspace.workspaceFolders && vscode.workspace.workspaceFolders.length > 1) {
                                vscode.window.showErrorMessage(`The language server for '${serverName}' crashed 5 times in the last 3 minutes. It will not be restarted.`);
                            }
                            else {
                                vscode.window.showErrorMessage(`The language server crashed 5 times in the last 3 minutes. It will not be restarted.`);
                            }
                            allClients.replace(this, false);
                        }
                        else {
                            this.crashTimes.shift();
                            let newClient = allClients.replace(this, true);
                            newClient.crashTimes = this.crashTimes;
                        }
                    }
                    return vscode_languageclient_1.CloseAction.DoNotRestart;
                }
            }
        };
        return new vscode_languageclient_1.LanguageClient(`cpptools: ${serverName}`, serverOptions, clientOptions);
    }
    onDidChangeSettings() {
        console.assert(Object.keys(previousCppSettings).length > 0);
        let filter = (key, val) => {
            return !(key in previousCppSettings) || val !== previousCppSettings[key];
        };
        let changedSettings = collectSettingsForTelemetry(filter, this.RootUri);
        if (Object.keys(changedSettings).length > 0) {
            if (changedSettings["commentContinuationPatterns"]) {
                extension_1.updateLanguageConfigurations();
            }
            telemetry.logLanguageServerEvent("CppSettingsChange", changedSettings, null);
        }
    }
    onDidChangeVisibleTextEditors(editors) {
        let settings = new settings_1.CppSettings(this.RootUri);
        if (settings.dimInactiveRegions) {
            for (let e of editors) {
                let valuePair = this.inactiveRegionsDecorations.get(e.document.uri.toString());
                if (valuePair) {
                    e.setDecorations(valuePair.decoration, valuePair.ranges);
                }
            }
        }
    }
    takeOwnership(document) {
        let params = {
            textDocument: {
                uri: document.uri.toString(),
                languageId: document.languageId,
                version: document.version,
                text: document.getText()
            }
        };
        this.notifyWhenReady(() => this.languageClient.sendNotification(DidOpenNotification, params));
        this.trackedDocuments.add(document);
    }
    requestWhenReady(request) {
        if (this.languageClient) {
            return request();
        }
        else if (this.isSupported && this.onReadyPromise) {
            return this.onReadyPromise.then(() => request());
        }
        else {
            return Promise.reject("Unsupported client");
        }
    }
    notifyWhenReady(notify) {
        if (this.languageClient) {
            notify();
        }
        else if (this.isSupported && this.onReadyPromise) {
            this.onReadyPromise.then(() => notify());
        }
    }
    registerNotifications() {
        console.assert(this.languageClient !== undefined, "This method must not be called until this.languageClient is set in \"onReady\"");
        this.languageClient.onNotification(ReloadWindowNotification, () => this.reloadWindow());
        this.languageClient.onNotification(LogTelemetryNotification, (e) => this.logTelemetry(e));
        this.languageClient.onNotification(ReportNavigationNotification, (e) => this.navigate(e));
        this.languageClient.onNotification(ReportStatusNotification, (e) => this.updateStatus(e));
        this.languageClient.onNotification(ReportTagParseStatusNotification, (e) => this.updateTagParseStatus(e));
        this.languageClient.onNotification(InactiveRegionNotification, (e) => this.updateInactiveRegions(e));
        this.setupOutputHandlers();
    }
    registerFileWatcher() {
        console.assert(this.languageClient !== undefined, "This method must not be called until this.languageClient is set in \"onReady\"");
        if (this.rootFolder) {
            this.rootPathFileWatcher = vscode.workspace.createFileSystemWatcher(path.join(this.RootPath, "*"), false, true, false);
            this.rootPathFileWatcher.onDidCreate((uri) => {
                this.languageClient.sendNotification(FileCreatedNotification, { uri: uri.toString() });
            });
            this.rootPathFileWatcher.onDidDelete((uri) => {
                this.languageClient.sendNotification(FileDeletedNotification, { uri: uri.toString() });
            });
            this.disposables.push(this.rootPathFileWatcher);
        }
        else {
            this.rootPathFileWatcher = undefined;
        }
    }
    setupOutputHandlers() {
        console.assert(this.languageClient !== undefined, "This method must not be called until this.languageClient is set in \"onReady\"");
        this.languageClient.onNotification(DebugProtocolNotification, (output) => {
            if (!this.debugChannel) {
                this.debugChannel = vscode.window.createOutputChannel(`C/C++ Debug Protocol: ${this.Name}`);
                this.disposables.push(this.debugChannel);
            }
            this.debugChannel.appendLine("");
            this.debugChannel.appendLine("************************************************************************************************************************");
            this.debugChannel.append(`${output}`);
        });
        this.languageClient.onNotification(DebugLogNotification, (output) => {
            if (!this.outputChannel) {
                if (vscode.workspace.workspaceFolders && vscode.workspace.workspaceFolders.length > 1) {
                    this.outputChannel = vscode.window.createOutputChannel(`C/C++: ${this.Name}`);
                }
                else {
                    this.outputChannel = logger.getOutputChannel();
                }
                this.disposables.push(this.outputChannel);
            }
            this.outputChannel.appendLine(`${output}`);
        });
    }
    reloadWindow() {
        let reload = "Reload";
        vscode.window.showInformationMessage("Reload the workspace for the settings change to take effect.", reload).then((value) => {
            if (value === reload) {
                vscode.commands.executeCommand("workbench.action.reloadWindow");
            }
        });
    }
    logTelemetry(notificationBody) {
        telemetry.logLanguageServerEvent(notificationBody.event, notificationBody.properties, notificationBody.metrics);
    }
    navigate(payload) {
        let cppSettings = new settings_1.CppSettings(this.RootUri);
        if (cppSettings.autoAddFileAssociations && payload.navigation.startsWith("<def")) {
            this.addFileAssociations(payload.navigation.substr(4));
            return;
        }
        let currentNavigation = payload.navigation;
        let maxLength = cppSettings.navigationLength;
        if (currentNavigation.length > maxLength) {
            currentNavigation = currentNavigation.substring(0, maxLength - 3).concat("...");
        }
        this.model.navigationLocation.Value = currentNavigation;
    }
    addFileAssociations(fileAssociations) {
        let settings = new settings_1.OtherSettings(this.RootUri);
        let assocs = settings.filesAssociations;
        let is_c = fileAssociations.startsWith("c");
        fileAssociations = fileAssociations.substr(is_c ? 3 : 2);
        let filesAndPaths = fileAssociations.split(";");
        let foundNewAssociation = false;
        for (let i = 0; i < filesAndPaths.length - 1; ++i) {
            let fileAndPath = filesAndPaths[i].split("@");
            let file = fileAndPath[0];
            let filePath = fileAndPath[1];
            if ((file in assocs) || (("**/" + file) in assocs)) {
                continue;
            }
            let j = file.lastIndexOf('.');
            if (j !== -1) {
                let ext = file.substr(j);
                if ((("*" + ext) in assocs) || (("**/*" + ext) in assocs)) {
                    continue;
                }
            }
            let foundGlobMatch = false;
            for (let assoc in assocs) {
                if (minimatch(filePath, assoc)) {
                    foundGlobMatch = true;
                    break;
                }
            }
            if (foundGlobMatch) {
                continue;
            }
            assocs[file] = is_c ? "c" : "cpp";
            foundNewAssociation = true;
        }
        if (foundNewAssociation) {
            settings.filesAssociations = assocs;
        }
    }
    updateStatus(notificationBody) {
        let message = notificationBody.status;
        util.setProgress(util.getProgressExecutableSuccess());
        if (message.endsWith("Indexing...")) {
            this.model.isTagParsing.Value = true;
        }
        else if (message.endsWith("Updating IntelliSense...")) {
            this.model.isUpdatingIntelliSense.Value = true;
        }
        else if (message.endsWith("IntelliSense Ready")) {
            this.model.isUpdatingIntelliSense.Value = false;
        }
        else if (message.endsWith("Ready")) {
            this.model.isTagParsing.Value = false;
            util.setProgress(util.getProgressParseRootSuccess());
        }
        else if (message.endsWith("No Squiggles")) {
            util.setIntelliSenseProgress(util.getProgressIntelliSenseNoSquiggles());
        }
        else if (message.endsWith("IntelliSense Fallback")) {
            let showIntelliSenseFallbackMessage = new persistentState_1.PersistentState("CPP.showIntelliSenseFallbackMessage", true);
            if (showIntelliSenseFallbackMessage.Value) {
                let learnMorePanel = "Learn More";
                let dontShowAgain = "Don't Show Again";
                vscode.window.showInformationMessage("Configure includePath for better IntelliSense results.", learnMorePanel, dontShowAgain).then((value) => {
                    switch (value) {
                        case learnMorePanel:
                            let uri = vscode.Uri.parse(`https://go.microsoft.com/fwlink/?linkid=864631`);
                            vscode.commands.executeCommand('vscode.open', uri);
                            vscode.commands.getCommands(true).then((commands) => {
                                if (commands.indexOf("workbench.action.problems.focus") >= 0) {
                                    vscode.commands.executeCommand("workbench.action.problems.focus");
                                }
                            });
                            break;
                        case dontShowAgain:
                            showIntelliSenseFallbackMessage.Value = false;
                            break;
                    }
                });
            }
        }
    }
    updateTagParseStatus(notificationBody) {
        this.model.tagParserStatus.Value = notificationBody.status;
    }
    updateInactiveRegions(params) {
        let renderOptions = {
            light: { color: "rgba(175,175,175,1.0)" },
            dark: { color: "rgba(155,155,155,1.0)" },
            rangeBehavior: vscode.DecorationRangeBehavior.ClosedOpen
        };
        let decoration = vscode.window.createTextEditorDecorationType(renderOptions);
        let ranges = [];
        params.regions.forEach(element => {
            let newRange = new vscode.Range(element.startLine, 0, element.endLine, 0);
            ranges.push(newRange);
        });
        let valuePair = this.inactiveRegionsDecorations.get(params.uri);
        if (valuePair) {
            valuePair.decoration.dispose();
            valuePair.decoration = decoration;
            valuePair.ranges = ranges;
        }
        else {
            let toInsert = {
                decoration: decoration,
                ranges: ranges
            };
            this.inactiveRegionsDecorations.set(params.uri, toInsert);
        }
        let settings = new settings_1.CppSettings(this.RootUri);
        if (settings.dimInactiveRegions) {
            let editors = vscode.window.visibleTextEditors.filter(e => e.document.uri.toString() === params.uri);
            for (let e of editors) {
                e.setDecorations(decoration, ranges);
            }
        }
    }
    requestGoToDeclaration() {
        return this.requestWhenReady(() => this.languageClient.sendRequest(GoToDeclarationRequest, null));
    }
    requestSwitchHeaderSource(rootPath, fileName) {
        let params = {
            rootPath: rootPath,
            switchHeaderSourceFileName: fileName
        };
        return this.requestWhenReady(() => this.languageClient.sendRequest(SwitchHeaderSourceRequest, params));
    }
    requestNavigationList(document) {
        return this.requestWhenReady(() => {
            return this.languageClient.sendRequest(NavigationListRequest, this.languageClient.code2ProtocolConverter.asTextDocumentIdentifier(document));
        });
    }
    activeDocumentChanged(document) {
        this.notifyWhenReady(() => {
            this.languageClient.sendNotification(ActiveDocumentChangeNotification, this.languageClient.code2ProtocolConverter.asTextDocumentIdentifier(document));
        });
    }
    activate() {
        for (let key in this.model) {
            if (this.model.hasOwnProperty(key)) {
                this.model[key].activate();
            }
        }
        this.resumeParsing();
    }
    selectionChanged(selection) {
        this.notifyWhenReady(() => this.languageClient.sendNotification(TextEditorSelectionChangeNotification, selection));
    }
    resetDatabase() {
        this.notifyWhenReady(() => this.languageClient.sendNotification(ResetDatabaseNotification));
    }
    deactivate() {
        for (let key in this.model) {
            if (this.model.hasOwnProperty(key)) {
                this.model[key].deactivate();
            }
        }
        this.pauseParsing();
    }
    pauseParsing() {
        this.notifyWhenReady(() => this.languageClient.sendNotification(PauseParsingNotification));
    }
    resumeParsing() {
        this.notifyWhenReady(() => this.languageClient.sendNotification(ResumeParsingNotification));
    }
    onConfigurationsChanged(configurations) {
        let params = {
            configurations: configurations,
            currentConfiguration: this.configuration.CurrentConfiguration
        };
        this.notifyWhenReady(() => {
            this.languageClient.sendNotification(ChangeFolderSettingsNotification, params);
            this.model.activeConfigName.Value = configurations[params.currentConfiguration].name;
        });
    }
    onSelectedConfigurationChanged(index) {
        let params = {
            currentConfiguration: index
        };
        this.notifyWhenReady(() => {
            this.languageClient.sendNotification(ChangeSelectedSettingNotification, params);
            this.model.activeConfigName.Value = this.configuration.ConfigurationNames[index];
        });
    }
    onCompileCommandsChanged(path) {
        let params = {
            uri: path
        };
        this.notifyWhenReady(() => this.languageClient.sendNotification(ChangeCompileCommandsNotification, params));
    }
    handleConfigurationSelectCommand() {
        this.notifyWhenReady(() => {
            ui.showConfigurations(this.configuration.ConfigurationNames)
                .then((index) => {
                if (index < 0) {
                    return;
                }
                this.configuration.select(index);
            });
        });
    }
    handleShowParsingCommands() {
        this.notifyWhenReady(() => {
            ui.showParsingCommands()
                .then((index) => {
                if (index === 0) {
                    this.pauseParsing();
                }
                else if (index === 1) {
                    this.resumeParsing();
                }
            });
        });
    }
    handleConfigurationEditCommand() {
        this.notifyWhenReady(() => this.configuration.handleConfigurationEditCommand(vscode.window.showTextDocument));
    }
    handleAddToIncludePathCommand(path) {
        this.notifyWhenReady(() => this.configuration.addToIncludePathCommand(path));
    }
    onInterval() {
        if (this.languageClient !== undefined && this.configuration !== undefined) {
            this.languageClient.sendNotification(IntervalTimerNotification);
            this.configuration.checkCppProperties();
        }
    }
    dispose() {
        let promise = (this.languageClient) ? this.languageClient.stop() : Promise.resolve();
        return promise.then(() => {
            this.disposables.forEach((d) => d.dispose());
            this.disposables = [];
            for (let key in this.model) {
                if (this.model.hasOwnProperty(key)) {
                    this.model[key].dispose();
                }
            }
        });
    }
}
function getLanguageServerFileName() {
    let extensionProcessName = 'Microsoft.VSCode.CPP.Extension';
    let plat = process.platform;
    if (plat === 'linux') {
        extensionProcessName += '.linux';
    }
    else if (plat === 'darwin') {
        extensionProcessName += '.darwin';
    }
    else if (plat === 'win32') {
        extensionProcessName += '.exe';
    }
    else {
        throw "Invalid Platform";
    }
    return path.resolve(util.getExtensionFilePath("bin"), extensionProcessName);
}
class NullClient {
    constructor() {
        this.booleanEvent = new vscode.EventEmitter();
        this.stringEvent = new vscode.EventEmitter();
        this.RootPath = "/";
        this.RootUri = vscode.Uri.file("/");
        this.Name = "(empty)";
        this.TrackedDocuments = new Set();
    }
    get TagParsingChanged() { return this.booleanEvent.event; }
    get IntelliSenseParsingChanged() { return this.booleanEvent.event; }
    get NavigationLocationChanged() { return this.stringEvent.event; }
    get TagParserStatusChanged() { return this.stringEvent.event; }
    get ActiveConfigChanged() { return this.stringEvent.event; }
    onDidChangeSettings() { }
    onDidChangeVisibleTextEditors(editors) { }
    takeOwnership(document) { }
    requestGoToDeclaration() { return Promise.resolve(); }
    requestSwitchHeaderSource(rootPath, fileName) { return Promise.resolve(""); }
    requestNavigationList(document) { return Promise.resolve(""); }
    activeDocumentChanged(document) { }
    activate() { }
    selectionChanged(selection) { }
    resetDatabase() { }
    deactivate() { }
    pauseParsing() { }
    resumeParsing() { }
    handleConfigurationSelectCommand() { }
    handleShowParsingCommands() { }
    handleConfigurationEditCommand() { }
    handleAddToIncludePathCommand(path) { }
    onInterval() { }
    dispose() {
        this.booleanEvent.dispose();
        this.stringEvent.dispose();
        return Promise.resolve();
    }
}
//# sourceMappingURL=client.js.map