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
const cpptoolsJsonUtils = require("./abTesting");
const DebuggerExtension = require("./Debugger/extension");
const fs = require("fs");
const LanguageServer = require("./LanguageServer/extension");
const os = require("os");
const Telemetry = require("./telemetry");
const util = require("./common");
const vscode = require("vscode");
const commands_1 = require("./commands");
const platform_1 = require("./platform");
const packageManager_1 = require("./packageManager");
const persistentState_1 = require("./LanguageServer/persistentState");
const installationInformation_1 = require("./installationInformation");
const logger_1 = require("./logger");
const releaseNotesVersion = 3;
function activate(context) {
    commands_1.initializeTemporaryCommandRegistrar();
    util.setExtensionContext(context);
    Telemetry.activate();
    util.setProgress(0);
    cpptoolsJsonUtils.activate(context);
    installationInformation_1.initializeInstallationInformation();
    DebuggerExtension.initialize();
    return processRuntimeDependencies();
}
exports.activate = activate;
function deactivate() {
    DebuggerExtension.dispose();
    Telemetry.deactivate();
    return LanguageServer.deactivate();
}
exports.deactivate = deactivate;
function processRuntimeDependencies() {
    return __awaiter(this, void 0, void 0, function* () {
        const installLockExists = yield util.checkInstallLockFile();
        if (installLockExists) {
            if (util.packageJson.activationEvents && util.packageJson.activationEvents.length === 1) {
                try {
                    yield offlineInstallation();
                }
                catch (error) {
                    logger_1.getOutputChannelLogger().showErrorMessage('The installation of the C/C++ extension failed. Please see the output window for more information.');
                    logger_1.showOutputChannel();
                }
            }
            else {
                yield finalizeExtensionActivation();
            }
        }
        else {
            try {
                yield onlineInstallation();
            }
            catch (error) {
                handleError(error);
            }
        }
    });
}
function offlineInstallation() {
    return __awaiter(this, void 0, void 0, function* () {
        installationInformation_1.setInstallationStage('getPlatformInfo');
        const info = yield platform_1.PlatformInformation.GetPlatformInformation();
        installationInformation_1.setInstallationStage('makeBinariesExecutable');
        yield makeBinariesExecutable();
        installationInformation_1.setInstallationStage('makeOfflineBinariesExecutable');
        yield makeOfflineBinariesExecutable(info);
        installationInformation_1.setInstallationStage('removeUnnecessaryFile');
        yield removeUnnecessaryFile();
        installationInformation_1.setInstallationStage('rewriteManifest');
        yield rewriteManifest();
        installationInformation_1.setInstallationStage('postInstall');
        yield postInstall(info);
    });
}
function onlineInstallation() {
    return __awaiter(this, void 0, void 0, function* () {
        installationInformation_1.setInstallationStage('getPlatformInfo');
        const info = yield platform_1.PlatformInformation.GetPlatformInformation();
        yield downloadAndInstallPackages(info);
        installationInformation_1.setInstallationStage('makeBinariesExecutable');
        yield makeBinariesExecutable();
        installationInformation_1.setInstallationStage('removeUnnecessaryFile');
        yield removeUnnecessaryFile();
        installationInformation_1.setInstallationStage('rewriteManifest');
        yield rewriteManifest();
        installationInformation_1.setInstallationStage('touchInstallLockFile');
        yield touchInstallLockFile();
        installationInformation_1.setInstallationStage('postInstall');
        yield postInstall(info);
    });
}
function downloadAndInstallPackages(info) {
    return __awaiter(this, void 0, void 0, function* () {
        let outputChannelLogger = logger_1.getOutputChannelLogger();
        outputChannelLogger.appendLine("Updating C/C++ dependencies...");
        let statusItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right);
        let packageManager = new packageManager_1.PackageManager(info, outputChannelLogger, statusItem);
        outputChannelLogger.appendLine('');
        installationInformation_1.setInstallationStage('downloadPackages');
        yield packageManager.DownloadPackages();
        outputChannelLogger.appendLine('');
        installationInformation_1.setInstallationStage('installPackages');
        yield packageManager.InstallPackages();
        statusItem.dispose();
    });
}
function makeBinariesExecutable() {
    return util.allowExecution(util.getDebugAdaptersPath("OpenDebugAD7"));
}
function makeOfflineBinariesExecutable(info) {
    let promises = [];
    let packages = util.packageJson["runtimeDependencies"];
    packages.forEach(p => {
        if (p.binaries && p.binaries.length > 0 &&
            p.platforms.findIndex(plat => plat === info.platform) !== -1 &&
            (p.architectures === undefined || p.architectures.findIndex(arch => arch === info.architecture) !== -1)) {
            p.binaries.forEach(binary => promises.push(util.allowExecution(util.getExtensionFilePath(binary))));
        }
    });
    return Promise.all(promises).then(() => { });
}
function removeUnnecessaryFile() {
    if (os.platform() !== 'win32') {
        let sourcePath = util.getDebugAdaptersPath("bin/OpenDebugAD7.exe.config");
        if (fs.existsSync(sourcePath)) {
            fs.rename(sourcePath, util.getDebugAdaptersPath("bin/OpenDebugAD7.exe.config.unused"), (err) => {
                if (err) {
                    logger_1.getOutputChannelLogger().appendLine(`ERROR: fs.rename failed with "${err.message}". Delete ${sourcePath} manually to enable debugging.`);
                }
            });
        }
    }
    return Promise.resolve();
}
function touchInstallLockFile() {
    return util.touchInstallLockFile();
}
function handleError(error) {
    let installationInformation = installationInformation_1.getInstallationInformationInstance();
    installationInformation.hasError = true;
    installationInformation.telemetryProperties['stage'] = installationInformation.stage;
    let errorMessage;
    if (error instanceof packageManager_1.PackageManagerError) {
        if (error instanceof packageManager_1.PackageManagerWebResponseError) {
            let webRequestPackageError = error;
            if (webRequestPackageError.socket) {
                let address = webRequestPackageError.socket.address();
                if (address) {
                    installationInformation.telemetryProperties['error.targetIP'] = address.address + ':' + address.port;
                }
            }
        }
        let packageError = error;
        installationInformation.telemetryProperties['error.methodName'] = packageError.methodName;
        installationInformation.telemetryProperties['error.message'] = packageError.message;
        if (packageError.innerError) {
            errorMessage = packageError.innerError.toString();
            installationInformation.telemetryProperties['error.innerError'] = util.removePotentialPII(errorMessage);
        }
        else {
            errorMessage = packageError.message;
        }
        if (packageError.pkg) {
            installationInformation.telemetryProperties['error.packageName'] = packageError.pkg.description;
            installationInformation.telemetryProperties['error.packageUrl'] = packageError.pkg.url;
        }
        if (packageError.errorCode) {
            installationInformation.telemetryProperties['error.errorCode'] = util.removePotentialPII(packageError.errorCode);
        }
    }
    else {
        errorMessage = error.toString();
        installationInformation.telemetryProperties['error.toString'] = util.removePotentialPII(errorMessage);
    }
    let outputChannelLogger = logger_1.getOutputChannelLogger();
    if (installationInformation.stage === 'downloadPackages') {
        outputChannelLogger.appendLine("");
    }
    outputChannelLogger.appendLine(`Failed at stage: ${installationInformation.stage}`);
    outputChannelLogger.appendLine(errorMessage);
    outputChannelLogger.appendLine("");
    outputChannelLogger.appendLine(`If you work in an offline environment or repeatedly see this error, try downloading a version of the extension with all the dependencies pre-included from https://github.com/Microsoft/vscode-cpptools/releases, then use the "Install from VSIX" command in VS Code to install it.`);
    logger_1.showOutputChannel();
}
function sendTelemetry(info) {
    let installBlob = installationInformation_1.getInstallationInformationInstance();
    const success = !installBlob.hasError;
    installBlob.telemetryProperties['success'] = success.toString();
    if (info.distribution) {
        installBlob.telemetryProperties['linuxDistroName'] = info.distribution.name;
        installBlob.telemetryProperties['linuxDistroVersion'] = info.distribution.version;
    }
    if (success) {
        util.setProgress(util.getProgressInstallSuccess());
        let versionShown = new persistentState_1.PersistentState("CPP.ReleaseNotesVersion", -1);
        if (versionShown.Value < releaseNotesVersion) {
            util.showReleaseNotes();
            versionShown.Value = releaseNotesVersion;
        }
    }
    installBlob.telemetryProperties['osArchitecture'] = info.architecture;
    Telemetry.logDebuggerEvent("acquisition", installBlob.telemetryProperties);
    return success;
}
function postInstall(info) {
    return __awaiter(this, void 0, void 0, function* () {
        let outputChannelLogger = logger_1.getOutputChannelLogger();
        outputChannelLogger.appendLine("");
        outputChannelLogger.appendLine("Finished installing dependencies");
        outputChannelLogger.appendLine("");
        const installSuccess = sendTelemetry(info);
        if (!installSuccess) {
            return Promise.reject("");
        }
        else {
            util.checkDistro(info);
            return finalizeExtensionActivation();
        }
    });
}
function finalizeExtensionActivation() {
    return __awaiter(this, void 0, void 0, function* () {
        const cpptoolsJsonFile = util.getExtensionFilePath("cpptools.json");
        try {
            const exists = yield util.checkFileExists(cpptoolsJsonFile);
            if (exists) {
                const cpptoolsString = yield util.readFileText(cpptoolsJsonFile);
                yield cpptoolsJsonUtils.processCpptoolsJson(cpptoolsString);
            }
        }
        catch (error) {
        }
        commands_1.getTemporaryCommandRegistrarInstance().activateLanguageServer();
        cpptoolsJsonUtils.downloadCpptoolsJsonPkg();
    });
}
function rewriteManifest() {
    util.packageJson.activationEvents = [
        "onLanguage:cpp",
        "onLanguage:c",
        "onCommand:extension.pickNativeProcess",
        "onCommand:extension.pickRemoteNativeProcess",
        "onCommand:C_Cpp.ConfigurationEdit",
        "onCommand:C_Cpp.ConfigurationSelect",
        "onCommand:C_Cpp.SwitchHeaderSource",
        "onCommand:C_Cpp.Navigate",
        "onCommand:C_Cpp.GoToDeclaration",
        "onCommand:C_Cpp.PeekDeclaration",
        "onCommand:C_Cpp.ToggleErrorSquiggles",
        "onCommand:C_Cpp.ToggleIncludeFallback",
        "onCommand:C_Cpp.ToggleDimInactiveRegions",
        "onCommand:C_Cpp.ShowReleaseNotes",
        "onCommand:C_Cpp.ResetDatabase",
        "onCommand:C_Cpp.PauseParsing",
        "onCommand:C_Cpp.ResumeParsing",
        "onCommand:C_Cpp.ShowParsingCommands",
        "onCommand:C_Cpp.TakeSurvey",
        "onDebug"
    ];
    return util.writeFileText(util.getPackageJsonPath(), util.getPackageJsonString());
}
//# sourceMappingURL=main.js.map