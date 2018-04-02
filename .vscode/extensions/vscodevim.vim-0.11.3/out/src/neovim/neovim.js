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
const child_process_1 = require("child_process");
const promised_neovim_client_1 = require("promised-neovim-client");
const vscode = require("vscode");
const configuration_1 = require("../configuration/configuration");
const register_1 = require("../register/register");
const textEditor_1 = require("../textEditor");
const position_1 = require("./../common/motion/position");
class Neovim {
    initialize() {
        return __awaiter(this, void 0, void 0, function* () {
            this.process = child_process_1.spawn(configuration_1.configuration.neovimPath, ['-u', 'NONE', '-N', '--embed'], {
                cwd: __dirname,
            });
            this.process.on('error', err => {
                console.log(err);
                vscode.window.showErrorMessage('Unable to setup neovim instance! Check your path.');
                configuration_1.configuration.enableNeovim = false;
            });
            this.nvim = yield promised_neovim_client_1.attach(this.process.stdin, this.process.stdout);
        });
    }
    run(vimState, command) {
        return __awaiter(this, void 0, void 0, function* () {
            yield this.syncVSToVim(vimState);
            command = ':' + command + '\n';
            command = command.replace('<', '<lt>');
            yield this.nvim.input(command);
            if ((yield this.nvim.getMode()).blocking) {
                yield this.nvim.input('<esc>');
            }
            yield this.syncVimToVs(vimState);
            return;
        });
    }
    input(vimState, keys) {
        return __awaiter(this, void 0, void 0, function* () {
            yield this.syncVSToVim(vimState);
            yield this.nvim.input(keys);
            yield this.syncVimToVs(vimState);
            return;
        });
    }
    // Data flows from VS to Vim
    syncVSToVim(vimState) {
        return __awaiter(this, void 0, void 0, function* () {
            const buf = yield this.nvim.getCurrentBuf();
            if (configuration_1.configuration.expandtab) {
                yield vscode.commands.executeCommand('editor.action.indentationToTabs');
            }
            yield this.nvim.setOption('gdefault', configuration_1.configuration.substituteGlobalFlag === true);
            yield buf.setLines(0, -1, true, textEditor_1.TextEditor.getText().split('\n'));
            const [rangeStart, rangeEnd] = [
                position_1.Position.EarlierOf(vimState.cursorPosition, vimState.cursorStartPosition),
                position_1.Position.LaterOf(vimState.cursorPosition, vimState.cursorStartPosition),
            ];
            yield this.nvim.callFunction('setpos', [
                '.',
                [0, vimState.cursorPosition.line + 1, vimState.cursorPosition.character, false],
            ]);
            yield this.nvim.callFunction('setpos', [
                "'<",
                [0, rangeStart.line + 1, rangeEnd.character, false],
            ]);
            yield this.nvim.callFunction('setpos', [
                "'>",
                [0, rangeEnd.line + 1, rangeEnd.character, false],
            ]);
            for (const mark of vimState.historyTracker.getMarks()) {
                yield this.nvim.callFunction('setpos', [
                    `'${mark.name}`,
                    [0, mark.position.line + 1, mark.position.character, false],
                ]);
            }
            // We only copy over " register for now, due to our weird handling of macros.
            let reg = yield register_1.Register.get(vimState);
            let vsRegTovimReg = [undefined, 'c', 'l', 'b'];
            yield this.nvim.callFunction('setreg', [
                '"',
                reg.text,
                vsRegTovimReg[vimState.effectiveRegisterMode],
            ]);
        });
    }
    // Data flows from Vim to VS
    syncVimToVs(vimState) {
        return __awaiter(this, void 0, void 0, function* () {
            const buf = yield this.nvim.getCurrentBuf();
            const lines = yield buf.getLines(0, -1, false);
            // one Windows, lines that went to nvim and back have a '\r' at the end,
            // which causes the issues exhibited in #1914
            const fixedLines = process.platform === 'win32' ? lines.map((line, index) => line.replace(/\r$/, '')) : lines;
            yield textEditor_1.TextEditor.replace(new vscode.Range(0, 0, textEditor_1.TextEditor.getLineCount() - 1, textEditor_1.TextEditor.getLineMaxColumn(textEditor_1.TextEditor.getLineCount() - 1)), fixedLines.join('\n'));
            console.log(`${lines.length} lines in nvim but ${textEditor_1.TextEditor.getLineCount()} in editor.`);
            let [row, character] = (yield this.nvim.callFunction('getpos', ['.'])).slice(1, 3);
            vimState.editor.selection = new vscode.Selection(new position_1.Position(row - 1, character), new position_1.Position(row - 1, character));
            if (configuration_1.configuration.expandtab) {
                yield vscode.commands.executeCommand('editor.action.indentationToSpaces');
            }
            // We're only syncing back the default register for now, due to the way we could
            // be storing macros in registers.
            const vimRegToVsReg = {
                v: register_1.RegisterMode.CharacterWise,
                V: register_1.RegisterMode.LineWise,
                '\x16': register_1.RegisterMode.BlockWise,
            };
            vimState.currentRegisterMode =
                vimRegToVsReg[(yield this.nvim.callFunction('getregtype', ['"']))];
            register_1.Register.put((yield this.nvim.callFunction('getreg', ['"'])), vimState);
        });
    }
    dispose() {
        if (this.nvim) {
            this.nvim.quit();
        }
        if (this.process) {
            this.process.kill();
        }
    }
}
exports.Neovim = Neovim;

//# sourceMappingURL=neovim.js.map
