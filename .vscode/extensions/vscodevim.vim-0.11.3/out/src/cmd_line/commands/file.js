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
const vscode = require("vscode");
const node = require("../node");
const untildify = require('untildify');
var FilePosition;
(function (FilePosition) {
    FilePosition[FilePosition["CurrentWindow"] = 0] = "CurrentWindow";
    FilePosition[FilePosition["NewWindow"] = 1] = "NewWindow";
})(FilePosition = exports.FilePosition || (exports.FilePosition = {}));
class FileCommand extends node.CommandBase {
    constructor(args) {
        super();
        this._name = 'file';
        this._arguments = args;
    }
    get arguments() {
        return this._arguments;
    }
    getActiveViewColumn() {
        const active = vscode.window.activeTextEditor;
        if (!active) {
            return vscode.ViewColumn.One;
        }
        return active.viewColumn;
    }
    getViewColumnToRight() {
        const active = vscode.window.activeTextEditor;
        if (!active) {
            return vscode.ViewColumn.One;
        }
        switch (active.viewColumn) {
            case vscode.ViewColumn.One:
                return vscode.ViewColumn.Two;
            case vscode.ViewColumn.Two:
                return vscode.ViewColumn.Three;
        }
        return active.viewColumn;
    }
    execute() {
        return __awaiter(this, void 0, void 0, function* () {
            if (this._arguments.bang) {
                yield vscode.commands.executeCommand('workbench.action.files.revert');
                return;
            }
            if (this._arguments.name === undefined) {
                // Open an empty file
                if (this._arguments.position === FilePosition.CurrentWindow) {
                    yield vscode.commands.executeCommand('workbench.action.files.newUntitledFile');
                }
                else {
                    yield vscode.commands.executeCommand('workbench.action.splitEditor');
                    yield vscode.commands.executeCommand('workbench.action.files.newUntitledFile');
                    yield vscode.commands.executeCommand('workbench.action.closeOtherEditors');
                }
                return;
            }
            else if (this._arguments.name === '') {
                if (this._arguments.position === FilePosition.NewWindow) {
                    yield vscode.commands.executeCommand('workbench.action.splitEditor');
                    return;
                }
                const fileList = yield vscode.window.showOpenDialog({});
                if (fileList) {
                    const doc = yield vscode.workspace.openTextDocument(fileList[0]);
                    vscode.window.showTextDocument(doc);
                }
                return;
            }
            let editorFilePath = vscode.window.activeTextEditor.document.uri.fsPath;
            this._arguments.name = untildify(this._arguments.name);
            let filePath = path.isAbsolute(this._arguments.name)
                ? this._arguments.name
                : path.join(path.dirname(editorFilePath), this._arguments.name);
            if (filePath !== editorFilePath) {
                if (!fs.existsSync(filePath)) {
                    // if file does not exist and does not have an extension
                    // try to find it with the same extension
                    if (path.extname(filePath) === '') {
                        const pathWithExt = filePath + path.extname(editorFilePath);
                        if (fs.existsSync(pathWithExt)) {
                            filePath = pathWithExt;
                        }
                        else {
                            // create file
                            fs.closeSync(fs.openSync(filePath, 'w'));
                        }
                    }
                }
                let folder = vscode.Uri.file(filePath);
                yield vscode.commands.executeCommand('vscode.open', folder, this._arguments.position === FilePosition.NewWindow
                    ? this.getViewColumnToRight()
                    : this.getActiveViewColumn());
                if (this.arguments.lineNumber) {
                    vscode.window.activeTextEditor.revealRange(new vscode.Range(new vscode.Position(this.arguments.lineNumber, 0), new vscode.Position(this.arguments.lineNumber, 0)));
                }
            }
        });
    }
}
exports.FileCommand = FileCommand;

//# sourceMappingURL=file.js.map
