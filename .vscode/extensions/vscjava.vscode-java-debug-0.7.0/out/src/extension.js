"use strict";
// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT license.
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
const vscode_extension_telemetry_1 = require("vscode-extension-telemetry");
const commands = require("./commands");
const configurationProvider_1 = require("./configurationProvider");
const constants_1 = require("./constants");
const hotCodeReplace_1 = require("./hotCodeReplace");
function activate(context) {
    // The reporter will be initialized by the later telemetry handler.
    let reporter = null;
    // Telemetry.
    const extensionPackage = require(context.asAbsolutePath("./package.json"));
    if (extensionPackage) {
        const packageInfo = {
            name: extensionPackage.name,
            version: extensionPackage.version,
            aiKey: extensionPackage.aiKey,
        };
        if (packageInfo.aiKey) {
            reporter = new vscode_extension_telemetry_1.default(packageInfo.name, packageInfo.version, packageInfo.aiKey);
            reporter.sendTelemetryEvent("activateExtension", {});
            const measureKeys = ["duration"];
            vscode.debug.onDidTerminateDebugSession(() => {
                fetchUsageData().then((ret) => {
                    if (Array.isArray(ret) && ret.length) {
                        ret.forEach((entry) => {
                            const commonProperties = {};
                            const measureProperties = {};
                            for (const key of Object.keys(entry)) {
                                if (measureKeys.indexOf(key) >= 0) {
                                    measureProperties[key] = entry[key];
                                }
                                else {
                                    commonProperties[key] = String(entry[key]);
                                }
                            }
                            reporter.sendTelemetryEvent(entry.scope === "exception" ? "exception" : "usageData", commonProperties, measureProperties);
                        });
                    }
                });
            });
        }
    }
    context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider("java", new configurationProvider_1.JavaDebugConfigurationProvider(reporter)));
    context.subscriptions.push(vscode.commands.registerCommand("JavaDebug.SpecifyProgramArgs", () => __awaiter(this, void 0, void 0, function* () {
        return specifyProgramArguments(context);
    })));
    hotCodeReplace_1.initializeHotCodeReplace(context);
    context.subscriptions.push(vscode.debug.onDidReceiveDebugSessionCustomEvent((customEvent) => {
        const t = customEvent.session ? customEvent.session.type : undefined;
        if (t !== constants_1.JAVA_LANGID) {
            return;
        }
        if (customEvent.event === constants_1.HCR_EVENT) {
            hotCodeReplace_1.handleHotCodeReplaceCustomEvent(customEvent);
        }
        else if (customEvent.event === constants_1.USER_NOTIFICATION_EVENT) {
            handleUserNotification(customEvent);
        }
    }));
}
exports.activate = activate;
// this method is called when your extension is deactivated
function deactivate() {
}
exports.deactivate = deactivate;
function handleUserNotification(customEvent) {
    if (customEvent.body.notificationType === "ERROR") {
        vscode.window.showErrorMessage(customEvent.body.message);
    }
    else if (customEvent.body.notificationType === "WARNING") {
        vscode.window.showWarningMessage(customEvent.body.message);
    }
    else {
        vscode.window.showInformationMessage(customEvent.body.message);
    }
}
function fetchUsageData() {
    return commands.executeJavaLanguageServerCommand(commands.JAVA_FETCH_USAGE_DATA);
}
function specifyProgramArguments(context) {
    const javaDebugProgramArgsKey = "JavaDebugProgramArgs";
    const options = {
        ignoreFocusOut: true,
        placeHolder: "Enter program arguments or leave empty to pass no args",
    };
    const prevArgs = context.workspaceState.get(javaDebugProgramArgsKey, "");
    if (prevArgs.length > 0) {
        options.value = prevArgs;
    }
    return vscode.window.showInputBox(options).then((text) => {
        // When user cancels the input box (by pressing Esc), the text value is undefined.
        if (text !== undefined) {
            context.workspaceState.update(javaDebugProgramArgsKey, text);
        }
        return text || " ";
    });
}
//# sourceMappingURL=extension.js.map