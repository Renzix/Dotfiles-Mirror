'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const LanguageServer = require("./LanguageServer/extension");
class TemporaryCommandRegistrar {
    constructor() {
        this.commandsToRegister = [
            "C_Cpp.ConfigurationEdit",
            "C_Cpp.ConfigurationSelect",
            "C_Cpp.SwitchHeaderSource",
            "C_Cpp.Navigate",
            "C_Cpp.GoToDeclaration",
            "C_Cpp.PeekDeclaration",
            "C_Cpp.ToggleErrorSquiggles",
            "C_Cpp.ToggleIncludeFallback",
            "C_Cpp.ToggleDimInactiveRegions",
            "C_Cpp.ShowReleaseNotes",
            "C_Cpp.ResetDatabase",
            "C_Cpp.PauseParsing",
            "C_Cpp.ResumeParsing",
            "C_Cpp.ShowParsingCommands",
            "C_Cpp.TakeSurvey"
        ];
        this.tempCommands = [];
        this.delayedCommandsToExecute = new Set();
        this.commandsToRegister.forEach(command => {
            this.registerTempCommand(command);
        });
    }
    registerTempCommand(command) {
        this.tempCommands.push(vscode.commands.registerCommand(command, () => {
            this.delayedCommandsToExecute.add(command);
        }));
    }
    activateLanguageServer() {
        this.tempCommands.forEach((command) => {
            command.dispose();
        });
        this.tempCommands = [];
        LanguageServer.activate(this.delayedCommandsToExecute.size > 0);
        this.delayedCommandsToExecute.forEach((command) => {
            vscode.commands.executeCommand(command);
        });
        this.delayedCommandsToExecute.clear();
    }
}
let tempCommandRegistrar;
function initializeTemporaryCommandRegistrar() {
    tempCommandRegistrar = new TemporaryCommandRegistrar();
}
exports.initializeTemporaryCommandRegistrar = initializeTemporaryCommandRegistrar;
function getTemporaryCommandRegistrarInstance() {
    return tempCommandRegistrar;
}
exports.getTemporaryCommandRegistrarInstance = getTemporaryCommandRegistrarInstance;
//# sourceMappingURL=commands.js.map