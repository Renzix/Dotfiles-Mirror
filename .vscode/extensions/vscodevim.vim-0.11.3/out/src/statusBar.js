"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
class StatusBarImpl {
    constructor() {
        this._statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left);
        this._prevModeName = undefined;
        this._isRecordingMacro = false;
    }
    SetText(text, mode, isRecordingMacro, forceUpdate = false) {
        let updateStatusBar = this._prevModeName !== mode || this._isRecordingMacro !== isRecordingMacro || forceUpdate;
        this._prevModeName = mode;
        this._isRecordingMacro = isRecordingMacro;
        if (updateStatusBar) {
            this._statusBarItem.text = text || '';
            this._statusBarItem.show();
        }
    }
    SetColor(background, foreground) {
        const currentColorCustomizations = vscode.workspace
            .getConfiguration('workbench')
            .get('colorCustomizations');
        const colorCustomizations = Object.assign(currentColorCustomizations || {}, {
            'statusBar.background': `${background}`,
            'statusBar.noFolderBackground': `${background}`,
            'statusBar.debuggingBackground': `${background}`,
            'statusBar.foreground': `${foreground}`,
        });
        if (foreground === undefined) {
            delete colorCustomizations['statusBar.foreground'];
        }
        vscode.workspace
            .getConfiguration('workbench')
            .update('colorCustomizations', colorCustomizations, true);
    }
    dispose() {
        this._statusBarItem.dispose();
    }
}
exports.StatusBar = new StatusBarImpl();

//# sourceMappingURL=statusBar.js.map
