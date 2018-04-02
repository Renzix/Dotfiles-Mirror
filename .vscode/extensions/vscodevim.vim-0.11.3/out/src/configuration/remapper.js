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
const _ = require("lodash");
const vscode = require("vscode");
const commandLine_1 = require("../cmd_line/commandLine");
const configuration_1 = require("../configuration/configuration");
const mode_1 = require("../mode/mode");
class Remappers {
    constructor() {
        this.remappers = [
            new InsertModeRemapper(true),
            new OtherModesRemapper(true),
            new InsertModeRemapper(false),
            new OtherModesRemapper(false),
        ];
    }
    get isPotentialRemap() {
        return _.some(this.remappers, r => r.isPotentialRemap);
    }
    sendKey(keys, modeHandler, vimState) {
        return __awaiter(this, void 0, void 0, function* () {
            let handled = false;
            for (let remapper of this.remappers) {
                handled = handled || (yield remapper.sendKey(keys, modeHandler, vimState));
            }
            return handled;
        });
    }
}
exports.Remappers = Remappers;
class Remapper {
    constructor(configKey, remappedModes, recursive) {
        this._remappings = [];
        /**
         * Have the keys pressed so far potentially be a remap
         */
        this._isPotentialRemap = false;
        this._recursive = recursive;
        this._remappedModes = remappedModes;
        this._remappings = configuration_1.configuration[configKey];
    }
    get isPotentialRemap() {
        return this._isPotentialRemap;
    }
    sendKey(keys, modeHandler, vimState) {
        return __awaiter(this, void 0, void 0, function* () {
            this._isPotentialRemap = false;
            if (this._remappedModes.indexOf(vimState.currentMode) === -1) {
                return false;
            }
            let remapping;
            const longestKeySequence = this._longestKeySequence();
            /**
             * Check to see if the keystrokes match any user-specified remapping.
             * In insert mode, we allow the users to precede the remapped command
             * with extraneous keystrokes (eg. "hello world jj").
             * In other modes, we have to precisely match the entire keysequence.
             */
            if (this._remappedModes.indexOf(mode_1.ModeName.Insert) === -1) {
                remapping = _.find(this._remappings, map => {
                    return map.before.join('') === keys.join('');
                });
            }
            else {
                for (let sliceLength = 1; sliceLength <= longestKeySequence; sliceLength++) {
                    const slice = keys.slice(-sliceLength);
                    const result = _.find(this._remappings, map => map.before.join('') === slice.join(''));
                    if (result) {
                        remapping = result;
                        break;
                    }
                }
            }
            if (remapping) {
                if (!this._recursive) {
                    vimState.isCurrentlyPerformingRemapping = true;
                }
                // Record length of remapped command
                vimState.recordedState.numberOfRemappedKeys += remapping.before.length;
                const numToRemove = remapping.before.length - 1;
                // Revert previously inserted characters
                // (e.g. jj remapped to esc, we have to revert the inserted "jj")
                if (this._remappedModes.indexOf(mode_1.ModeName.Insert) >= 0) {
                    // Revert every single inserted character.
                    // We subtract 1 because we haven't actually applied the last key.
                    yield vimState.historyTracker.undoAndRemoveChanges(Math.max(0, numToRemove * vimState.allCursors.length));
                    vimState.cursorPosition = vimState.cursorPosition.getLeft(numToRemove);
                }
                // We need to remove the keys that were remapped into different keys
                // from the state.
                vimState.recordedState.actionKeys = vimState.recordedState.actionKeys.slice(0, -numToRemove);
                vimState.keyHistory = vimState.keyHistory.slice(0, -numToRemove);
                if (remapping.after) {
                    const count = vimState.recordedState.count || 1;
                    vimState.recordedState.count = 0;
                    for (let i = 0; i < count; i++) {
                        yield modeHandler.handleMultipleKeyEvents(remapping.after);
                    }
                }
                if (remapping.commands) {
                    for (const command of remapping.commands) {
                        // Check if this is a vim command by looking for :
                        if (command.command.slice(0, 1) === ':') {
                            yield commandLine_1.CommandLine.Run(command.command.slice(1, command.command.length), modeHandler.vimState);
                            yield modeHandler.updateView(modeHandler.vimState);
                        }
                        else {
                            yield vscode.commands.executeCommand(command.command, command.args);
                        }
                    }
                }
                vimState.isCurrentlyPerformingRemapping = false;
                return true;
            }
            // Check to see if a remapping could potentially be applied when more keys are received
            for (let remap of this._remappings) {
                if (keys.join('') === remap.before.slice(0, keys.length).join('')) {
                    this._isPotentialRemap = true;
                    break;
                }
            }
            return false;
        });
    }
    _longestKeySequence() {
        if (this._remappings.length > 0) {
            return _.maxBy(this._remappings, map => map.before.length).before.length;
        }
        else {
            return 1;
        }
    }
}
class InsertModeRemapper extends Remapper {
    constructor(recursive) {
        super('insertModeKeyBindings' + (recursive ? '' : 'NonRecursive'), [mode_1.ModeName.Insert], recursive);
    }
}
class OtherModesRemapper extends Remapper {
    constructor(recursive) {
        super('otherModesKeyBindings' + (recursive ? '' : 'NonRecursive'), [mode_1.ModeName.Normal, mode_1.ModeName.Visual, mode_1.ModeName.VisualLine, mode_1.ModeName.VisualBlock], recursive);
    }
}

//# sourceMappingURL=remapper.js.map
