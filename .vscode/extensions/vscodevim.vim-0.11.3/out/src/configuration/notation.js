"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const _ = require("lodash");
class Notation {
    /**
     * Normalizes key to AngleBracketNotation
     * (e.g. <ctrl+x>, Ctrl+x, <c-x> normalized to <C-x>)
     * and resolves special cases such as '<leader>'
     */
    static NormalizeKey(key, leaderKey) {
        if (!this.isSurroundedByAngleBrackets(key) && key.length > 1) {
            key = `<${key.toLocaleLowerCase()}>`;
        }
        // Special cases that we handle incorrectly (internally)
        if (key.toLocaleLowerCase() === '<space>') {
            return ' ';
        }
        if (key.toLocaleLowerCase() === '<cr>') {
            return '\n';
        }
        if (key.toLocaleLowerCase() === '<leader>') {
            return leaderKey;
        }
        if (_.includes(['<up>', '<down>', '<left>', '<right>'], key.toLocaleLowerCase())) {
            key = key.toLocaleLowerCase();
        }
        for (const notationMapKey in this._notationMap) {
            if (this._notationMap.hasOwnProperty(notationMapKey)) {
                const regex = new RegExp(this._notationMap[notationMapKey].join('|'), 'gi');
                if (regex.test(key)) {
                    key = key.replace(regex, notationMapKey);
                    break;
                }
            }
        }
        return key;
    }
    static isSurroundedByAngleBrackets(key) {
        return key.startsWith('<') && key.endsWith('>');
    }
}
// Mapping from the nomalized string to regex strings that could match it.
Notation._notationMap = {
    'C-': ['ctrl\\+', 'c\\-'],
    'D-': ['cmd\\+', 'd\\-'],
    Esc: ['escape', 'esc'],
    BS: ['backspace', 'bs'],
    Del: ['delete', 'del'],
};
exports.Notation = Notation;

//# sourceMappingURL=notation.js.map
