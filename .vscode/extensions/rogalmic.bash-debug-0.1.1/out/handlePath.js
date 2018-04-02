"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
/**
 * @example <caption>Undefined path stays undefined.</caption>
 * // can't execute jsdoctest
 * // expandPath(undefined, "C:\\Users\\wsh\proj0"); // => undefined
 *
 * @example <caption>Absolute path is not replaced.</caption>
 * expandPath("C:\\Users\\wsh\\proj0\\path\\to\\script.sh", "C:\\Users\\wsh\proj0");
 * // => "C:/Users/wsh/proj0/path/to/script.sh"
 *
 * @example <caption>Using {workspaceFolder}, on windows</caption>
 * expandPath("{workspaceFolder}\\path\\to\\script.sh", "C:\\Users\\wsh\\proj0");
 * // => "C:/Users/wsh/proj0/path/to/script.sh"
 */
function expandPath(path, rootPath) {
    if (!path) {
        return undefined;
    }
    ;
    if (rootPath) {
        path = path.replace("{workspaceFolder}", rootPath).split("\\").join("/");
    }
    return path;
}
exports.expandPath = expandPath;
/**
 * @example <caption>Undefined path stays undefined.</caption>
 * // can't execute jsdoctest
 * // getWSLPath(undefined); // => undefined
 *
 * @example <caption>If windows path, WSL path conversion</caption>
 * getWSLPath("C:\\Users\\wsh\\proj0\\path\\to\\script.sh");
 * // => "/mnt/c/Users/wsh/proj0/path/to/script.sh"
 *
 * @example <caption>If path starts with "/", no WSL path conversion</caption>
 * getWSLPath("/mnt/c/Users/wsh/proj0/path/to/script.sh");
 * // => "/mnt/c/Users/wsh/proj0/path/to/script.sh"
 */
function getWSLPath(path) {
    if (!path) {
        return undefined;
    }
    ;
    if (!path.startsWith("/")) {
        path = "/mnt/" + path.substr(0, 1).toLowerCase() + path.substr("X:".length).split("\\").join("/");
    }
    return path;
}
exports.getWSLPath = getWSLPath;
/**
 * @example <caption>Absolute path</caption>
 * reverseWSLPath("/mnt/c/Users/wsh/proj0/path/to/script.sh");
 * // => "C:\\Users\\wsh\\proj0\\path\\to\\script.sh"
 */
function reverseWSLPath(wslPath) {
    return wslPath.substr("/mnt/".length, 1).toUpperCase() + ":" + wslPath.substr("/mnt/".length + 1).split("/").join("\\");
}
exports.reverseWSLPath = reverseWSLPath;
//# sourceMappingURL=handlePath.js.map