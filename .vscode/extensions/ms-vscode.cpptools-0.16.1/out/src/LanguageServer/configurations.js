'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const path = require("path");
const fs = require("fs");
const vscode = require("vscode");
const util = require("../common");
const persistentState_1 = require("./persistentState");
const configVersion = 3;
let defaultSettings = `{
    "configurations": [
        {
            "name": "Mac",
            "includePath": [
                "/usr/include",
                "/usr/local/include",
                "$\{workspaceFolder\}"
            ],
            "defines": [],
            "intelliSenseMode": "clang-x64",
            "browse": {
                "path": [
                    "/usr/include",
                    "/usr/local/include",
                    "$\{workspaceFolder\}"
                ],
                "limitSymbolsToIncludedHeaders": true,
                "databaseFilename": ""
            },
            "macFrameworkPath": [
                "/System/Library/Frameworks",
                "/Library/Frameworks"
            ]
        },
        {
            "name": "Linux",
            "includePath": [
                "/usr/include",
                "/usr/local/include",
                "$\{workspaceFolder\}"
            ],
            "defines": [],
            "intelliSenseMode": "clang-x64",
            "browse": {
                "path": [
                    "/usr/include",
                    "/usr/local/include",
                    "$\{workspaceFolder\}"
                ],
                "limitSymbolsToIncludedHeaders": true,
                "databaseFilename": ""
            }
        },
        {
            "name": "Win32",
            "includePath": [
                "C:/Program Files (x86)/Microsoft Visual Studio 14.0/VC/include",
                "$\{workspaceFolder\}"
            ],
            "defines": [
                "_DEBUG",
                "UNICODE",
                "_UNICODE"
            ],
            "intelliSenseMode": "msvc-x64",
            "browse": {
                "path": [
                    "C:/Program Files (x86)/Microsoft Visual Studio 14.0/VC/include/*",
                    "$\{workspaceFolder\}"
                ],
                "limitSymbolsToIncludedHeaders": true,
                "databaseFilename": ""
            }
        }
    ],
    "version": ${configVersion}
}
`;
class CppProperties {
    constructor(rootPath) {
        this.propertiesFile = null;
        this.configurationJson = null;
        this.configFileWatcher = null;
        this.configFileWatcherFallbackTime = new Date();
        this.compileCommandFileWatchers = [];
        this.defaultCompilerPath = null;
        this.defaultCStandard = null;
        this.defaultCppStandard = null;
        this.defaultIncludes = null;
        this.defaultFrameworks = null;
        this.configurationGlobPattern = "**/c_cpp_properties.json";
        this.disposables = [];
        this.configurationsChanged = new vscode.EventEmitter();
        this.selectionChanged = new vscode.EventEmitter();
        this.compileCommandsChanged = new vscode.EventEmitter();
        this.configurationIncomplete = true;
        console.assert(rootPath !== undefined);
        this.currentConfigurationIndex = new persistentState_1.PersistentFolderState("CppProperties.currentConfigurationIndex", -1, rootPath);
        this.configFolder = path.join(rootPath, ".vscode");
        this.resetToDefaultSettings(this.currentConfigurationIndex.Value === -1);
        let configFilePath = path.join(this.configFolder, "c_cpp_properties.json");
        if (fs.existsSync(configFilePath)) {
            this.propertiesFile = vscode.Uri.file(configFilePath);
        }
        this.configFileWatcher = vscode.workspace.createFileSystemWatcher(path.join(this.configFolder, this.configurationGlobPattern));
        this.disposables.push(this.configFileWatcher);
        this.configFileWatcher.onDidCreate((uri) => {
            this.propertiesFile = uri;
            this.handleConfigurationChange();
        });
        this.configFileWatcher.onDidDelete(() => {
            this.propertiesFile = null;
            this.resetToDefaultSettings(true);
            this.handleConfigurationChange();
        });
        this.configFileWatcher.onDidChange(() => {
            this.handleConfigurationChange();
        });
        this.disposables.push(vscode.Disposable.from(this.configurationsChanged, this.selectionChanged, this.compileCommandsChanged));
    }
    get ConfigurationsChanged() { return this.configurationsChanged.event; }
    get SelectionChanged() { return this.selectionChanged.event; }
    get CompileCommandsChanged() { return this.compileCommandsChanged.event; }
    get Configurations() { return this.configurationJson.configurations; }
    get CurrentConfiguration() { return this.currentConfigurationIndex.Value; }
    get ConfigurationNames() {
        let result = [];
        this.configurationJson.configurations.forEach((config) => result.push(config.name));
        return result;
    }
    set CompilerDefaults(compilerDefaults) {
        this.defaultCompilerPath = compilerDefaults.compilerPath;
        this.defaultCStandard = compilerDefaults.cStandard;
        this.defaultCppStandard = compilerDefaults.cppStandard;
        this.defaultIncludes = compilerDefaults.includes;
        this.defaultFrameworks = compilerDefaults.frameworks;
        this.handleConfigurationChange();
    }
    onConfigurationsChanged() {
        this.configurationsChanged.fire(this.Configurations);
    }
    onSelectionChanged() {
        this.selectionChanged.fire(this.CurrentConfiguration);
    }
    onCompileCommandsChanged(path) {
        this.compileCommandsChanged.fire(path);
    }
    resetToDefaultSettings(resetIndex) {
        this.configurationJson = JSON.parse(defaultSettings);
        if (resetIndex || this.CurrentConfiguration < 0 ||
            this.CurrentConfiguration >= this.configurationJson.configurations.length) {
            this.currentConfigurationIndex.Value = this.getConfigIndexForPlatform(this.configurationJson);
        }
        this.configurationIncomplete = true;
    }
    applyDefaultIncludePathsAndFrameworks() {
        if (this.configurationIncomplete && this.defaultIncludes && this.defaultFrameworks) {
            this.configurationJson.configurations[this.CurrentConfiguration].includePath = this.defaultIncludes;
            this.configurationJson.configurations[this.CurrentConfiguration].browse.path = this.defaultIncludes;
            if (process.platform === 'darwin') {
                this.configurationJson.configurations[this.CurrentConfiguration].macFrameworkPath = this.defaultFrameworks;
            }
            if (this.defaultCompilerPath) {
                this.configurationJson.configurations[this.CurrentConfiguration].compilerPath = this.defaultCompilerPath;
            }
            if (this.defaultCStandard) {
                this.configurationJson.configurations[this.CurrentConfiguration].cStandard = this.defaultCStandard;
            }
            if (this.defaultCppStandard) {
                this.configurationJson.configurations[this.CurrentConfiguration].cppStandard = this.defaultCppStandard;
            }
            this.configurationIncomplete = false;
        }
    }
    getConfigIndexForPlatform(config) {
        if (this.configurationJson.configurations.length > 3) {
            return this.configurationJson.configurations.length - 1;
        }
        let nodePlatform = process.platform;
        let plat;
        if (nodePlatform === 'linux') {
            plat = "Linux";
        }
        else if (nodePlatform === 'darwin') {
            plat = "Mac";
        }
        else if (nodePlatform === 'win32') {
            plat = "Win32";
        }
        for (let i = 0; i < this.configurationJson.configurations.length; i++) {
            if (config.configurations[i].name === plat) {
                return i;
            }
        }
        return this.configurationJson.configurations.length - 1;
    }
    getIntelliSenseModeForPlatform(name) {
        if (name === "Linux" || name === "Mac") {
            return "clang-x64";
        }
        else if (name === "Win32") {
            return "msvc-x64";
        }
        else {
            let nodePlatform = process.platform;
            if (nodePlatform === 'linux' || nodePlatform === 'darwin') {
                return "clang-x64";
            }
        }
        return "msvc-x64";
    }
    includePathConverted() {
        for (let i = 0; i < this.configurationJson.configurations.length; i++) {
            if (this.configurationJson.configurations[i].browse === undefined || this.configurationJson.configurations[i].browse.path === undefined) {
                return false;
            }
        }
        return true;
    }
    addToIncludePathCommand(path) {
        this.handleConfigurationEditCommand((document) => {
            let config = this.configurationJson.configurations[this.CurrentConfiguration];
            config.includePath.splice(config.includePath.length, 0, path);
            fs.writeFileSync(this.propertiesFile.fsPath, JSON.stringify(this.configurationJson, null, 4));
            this.updateServerOnFolderSettingsChange();
        });
    }
    select(index) {
        if (index === this.configurationJson.configurations.length) {
            this.handleConfigurationEditCommand(vscode.window.showTextDocument);
            return;
        }
        this.currentConfigurationIndex.Value = index;
        this.onSelectionChanged();
    }
    resolveAndSplit(paths) {
        let result = [];
        if (paths) {
            paths.forEach(entry => {
                let entries = util.resolveVariables(entry).split(";").filter(e => e);
                result = result.concat(entries);
            });
        }
        return result;
    }
    updateServerOnFolderSettingsChange() {
        for (let i = 0; i < this.configurationJson.configurations.length; i++) {
            let configuration = this.configurationJson.configurations[i];
            if (configuration.includePath) {
                configuration.includePath = this.resolveAndSplit(configuration.includePath);
            }
            if (configuration.browse && configuration.browse.path) {
                configuration.browse.path = this.resolveAndSplit(configuration.browse.path);
            }
            if (configuration.macFrameworkPath) {
                configuration.macFrameworkPath = this.resolveAndSplit(configuration.macFrameworkPath);
            }
            if (configuration.forcedInclude) {
                configuration.forcedInclude = this.resolveAndSplit(configuration.forcedInclude);
            }
            if (configuration.compileCommands) {
                configuration.compileCommands = util.resolveVariables(configuration.compileCommands);
            }
            if (configuration.compilerPath) {
                configuration.compilerPath = util.resolveVariables(configuration.compilerPath);
            }
        }
        this.updateCompileCommandsFileWatchers();
        if (!this.configurationIncomplete) {
            this.onConfigurationsChanged();
        }
    }
    updateCompileCommandsFileWatchers() {
        this.compileCommandFileWatchers.forEach((watcher) => watcher.close());
        this.compileCommandFileWatchers = [];
        let filePaths = new Set();
        this.configurationJson.configurations.forEach(c => {
            if (c.compileCommands !== undefined && fs.existsSync(c.compileCommands)) {
                filePaths.add(c.compileCommands);
            }
        });
        try {
            filePaths.forEach((path) => {
                this.compileCommandFileWatchers.push(fs.watch(path, (event, filename) => {
                    if (event !== "rename") {
                        this.onCompileCommandsChanged(path);
                    }
                }));
            });
        }
        catch (e) {
        }
    }
    handleConfigurationEditCommand(onSuccess) {
        if (this.propertiesFile && fs.existsSync(this.propertiesFile.fsPath)) {
            vscode.workspace.openTextDocument(this.propertiesFile).then((document) => {
                onSuccess(document);
            });
        }
        else {
            fs.mkdir(this.configFolder, (e) => {
                if (!e || e.code === 'EEXIST') {
                    let dirPathEscaped = this.configFolder.replace("#", "%23");
                    let fullPathToFile = path.join(dirPathEscaped, "c_cpp_properties.json");
                    let filePath = vscode.Uri.parse("untitled:" + fullPathToFile);
                    vscode.workspace.openTextDocument(filePath).then((document) => {
                        let edit = new vscode.WorkspaceEdit();
                        if (this.configurationJson === undefined) {
                            this.resetToDefaultSettings(true);
                        }
                        this.applyDefaultIncludePathsAndFrameworks();
                        edit.insert(document.uri, new vscode.Position(0, 0), JSON.stringify(this.configurationJson, null, 4));
                        vscode.workspace.applyEdit(edit).then((status) => {
                            document.save().then(() => {
                                this.propertiesFile = vscode.Uri.file(path.join(this.configFolder, "c_cpp_properties.json"));
                                vscode.workspace.openTextDocument(this.propertiesFile).then((document) => {
                                    onSuccess(document);
                                });
                            });
                        });
                    });
                }
            });
        }
    }
    handleConfigurationChange() {
        this.configFileWatcherFallbackTime = new Date();
        if (this.propertiesFile) {
            this.parsePropertiesFile();
            if (this.configurationJson !== undefined) {
                if (this.CurrentConfiguration < 0 ||
                    this.CurrentConfiguration >= this.configurationJson.configurations.length) {
                    this.currentConfigurationIndex.Value = this.getConfigIndexForPlatform(this.configurationJson);
                }
            }
        }
        if (this.configurationJson === undefined) {
            this.resetToDefaultSettings(true);
        }
        this.applyDefaultIncludePathsAndFrameworks();
        this.updateServerOnFolderSettingsChange();
    }
    parsePropertiesFile() {
        try {
            let readResults = fs.readFileSync(this.propertiesFile.fsPath, 'utf8');
            if (readResults === "") {
                return;
            }
            let newJson = JSON.parse(readResults);
            if (!newJson || !newJson.configurations || newJson.configurations.length === 0) {
                throw { message: "Invalid configuration file. There must be at least one configuration present in the array." };
            }
            if (!this.configurationIncomplete && this.configurationJson && this.configurationJson.configurations &&
                this.CurrentConfiguration < this.configurationJson.configurations.length) {
                for (let i = 0; i < newJson.configurations.length; i++) {
                    if (newJson.configurations[i].name === this.configurationJson.configurations[this.CurrentConfiguration].name) {
                        this.currentConfigurationIndex.Value = i;
                        break;
                    }
                }
            }
            this.configurationJson = newJson;
            if (this.CurrentConfiguration >= newJson.configurations.length) {
                this.currentConfigurationIndex.Value = this.getConfigIndexForPlatform(newJson);
            }
            this.configurationIncomplete = false;
            let dirty = false;
            for (let i = 0; i < this.configurationJson.configurations.length; i++) {
                let config = this.configurationJson.configurations[i];
                if (config.intelliSenseMode === undefined) {
                    dirty = true;
                    config.intelliSenseMode = this.getIntelliSenseModeForPlatform(config.name);
                }
            }
            if (this.configurationJson.version !== configVersion) {
                dirty = true;
                if (this.configurationJson.version === undefined) {
                    this.updateToVersion2();
                }
                if (this.configurationJson.version === 2) {
                    this.updateToVersion3();
                }
                else {
                    this.configurationJson.version = configVersion;
                    vscode.window.showErrorMessage('Unknown version number found in c_cpp_properties.json. Some features may not work as expected.');
                }
            }
            let config = this.configurationJson.configurations[this.CurrentConfiguration];
            if (config.compilerPath === undefined && this.defaultCompilerPath && !config.compileCommands) {
                config.compilerPath = this.defaultCompilerPath;
                dirty = true;
            }
            if (!config.cStandard && this.defaultCStandard) {
                config.cStandard = this.defaultCStandard;
                dirty = true;
            }
            if (!config.cppStandard && this.defaultCppStandard) {
                config.cppStandard = this.defaultCppStandard;
                dirty = true;
            }
            if (dirty) {
                fs.writeFileSync(this.propertiesFile.fsPath, JSON.stringify(this.configurationJson, null, 4));
            }
        }
        catch (err) {
            vscode.window.showErrorMessage('Failed to parse "' + this.propertiesFile.fsPath + '": ' + err.message);
            throw err;
        }
    }
    updateToVersion2() {
        this.configurationJson.version = 2;
        if (!this.includePathConverted()) {
            for (let i = 0; i < this.configurationJson.configurations.length; i++) {
                let config = this.configurationJson.configurations[i];
                if (config.browse === undefined) {
                    config.browse = {};
                }
                if (config.browse.path === undefined && (this.defaultIncludes !== undefined || config.includePath !== undefined)) {
                    config.browse.path = (config.includePath === undefined) ? this.defaultIncludes.slice(0) : config.includePath.slice(0);
                }
            }
        }
    }
    updateToVersion3() {
        this.configurationJson.version = 3;
        for (let i = 0; i < this.configurationJson.configurations.length; i++) {
            let config = this.configurationJson.configurations[i];
            if (config.name === "Mac" || (process.platform === "darwin" && config.name !== "Win32" && config.name !== "Linux")) {
                if (config.macFrameworkPath === undefined) {
                    config.macFrameworkPath = [
                        "/System/Library/Frameworks",
                        "/Library/Frameworks"
                    ];
                }
            }
        }
    }
    checkCppProperties() {
        let propertiesFile = path.join(this.configFolder, "c_cpp_properties.json");
        fs.stat(propertiesFile, (err, stats) => {
            if (err) {
                if (this.propertiesFile !== null) {
                    this.propertiesFile = null;
                    this.resetToDefaultSettings(true);
                    this.handleConfigurationChange();
                }
            }
            else if (stats.mtime > this.configFileWatcherFallbackTime) {
                if (this.propertiesFile === null) {
                    this.propertiesFile = vscode.Uri.file(propertiesFile);
                }
                this.handleConfigurationChange();
            }
        });
    }
    dispose() {
        this.disposables.forEach((d) => d.dispose());
        this.disposables = [];
        this.compileCommandFileWatchers.forEach((watcher) => watcher.close());
        this.compileCommandFileWatchers = [];
    }
}
exports.CppProperties = CppProperties;
//# sourceMappingURL=configurations.js.map