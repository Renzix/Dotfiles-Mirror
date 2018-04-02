"use strict";
// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT license.
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const constants_1 = require("./constants");
const suppressedReasons = new Set();
const YES_BUTTON = "Yes";
const NO_BUTTON = "No";
const NEVER_BUTTON = "Not show again";
var HcrChangeType;
(function (HcrChangeType) {
    HcrChangeType["ERROR"] = "ERROR";
    HcrChangeType["WARNING"] = "WARNING";
    HcrChangeType["STARTING"] = "STARTING";
    HcrChangeType["END"] = "END";
    HcrChangeType["BUILD_COMPLETE"] = "BUILD_COMPLETE";
})(HcrChangeType || (HcrChangeType = {}));
function initializeHotCodeReplace(context) {
    context.subscriptions.push(vscode.debug.onDidTerminateDebugSession((session) => {
        const t = session ? session.type : undefined;
        if (t === constants_1.JAVA_LANGID) {
            suppressedReasons.clear();
        }
    }));
}
exports.initializeHotCodeReplace = initializeHotCodeReplace;
function handleHotCodeReplaceCustomEvent(hcrEvent) {
    if (hcrEvent.body.changeType === HcrChangeType.BUILD_COMPLETE) {
        return vscode.window.withProgress({ location: vscode.ProgressLocation.Window }, (progress) => {
            progress.report({ message: "Applying code changes..." });
            return hcrEvent.session.customRequest("redefineClasses");
        });
    }
    if (hcrEvent.body.changeType === HcrChangeType.ERROR || hcrEvent.body.changeType === HcrChangeType.WARNING) {
        if (!suppressedReasons.has(hcrEvent.body.message)) {
            vscode.window.showInformationMessage(`Hot code replace failed - ${hcrEvent.body.message}. Would you like to restart the debug session?`, YES_BUTTON, NO_BUTTON, NEVER_BUTTON).then((res) => {
                if (res === NEVER_BUTTON) {
                    suppressedReasons.add(hcrEvent.body.message);
                }
                else if (res === YES_BUTTON) {
                    vscode.commands.executeCommand("workbench.action.debug.restart");
                }
            });
        }
    }
}
exports.handleHotCodeReplaceCustomEvent = handleHotCodeReplaceCustomEvent;
//# sourceMappingURL=hotCodeReplace.js.map