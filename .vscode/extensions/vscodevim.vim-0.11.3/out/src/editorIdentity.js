"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
class EditorIdentity {
    constructor(textEditor) {
        this._fileName = (textEditor && textEditor.document && textEditor.document.fileName) || '';
        this._viewColumn = (textEditor && textEditor.viewColumn) || vscode.ViewColumn.One;
    }
    get fileName() {
        return this._fileName;
    }
    get viewColumn() {
        return this._viewColumn;
    }
    hasSameBuffer(identity) {
        return this.fileName === identity.fileName;
    }
    isEqual(other) {
        return this.fileName === other.fileName && this.viewColumn === other.viewColumn;
    }
    toString() {
        return this.fileName + this.viewColumn;
    }
}
exports.EditorIdentity = EditorIdentity;

//# sourceMappingURL=editorIdentity.js.map
