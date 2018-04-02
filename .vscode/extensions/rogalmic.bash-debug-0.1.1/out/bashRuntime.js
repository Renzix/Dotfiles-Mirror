"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const child_process_1 = require("child_process");
var validatePathResult;
(function (validatePathResult) {
    validatePathResult[validatePathResult["success"] = 0] = "success";
    validatePathResult[validatePathResult["notExistCwd"] = 1] = "notExistCwd";
    validatePathResult[validatePathResult["notFoundBash"] = 2] = "notFoundBash";
    validatePathResult[validatePathResult["notFoundBashdb"] = 3] = "notFoundBashdb";
    validatePathResult[validatePathResult["notFoundCat"] = 4] = "notFoundCat";
    validatePathResult[validatePathResult["notFoundMkfifo"] = 5] = "notFoundMkfifo";
    validatePathResult[validatePathResult["notFoundPkill"] = 6] = "notFoundPkill";
    validatePathResult[validatePathResult["timeout"] = 7] = "timeout";
    validatePathResult[validatePathResult["unknown"] = 8] = "unknown";
})(validatePathResult || (validatePathResult = {}));
/**
 * @example
 * _validatePath("./", "bash", "bashdb", "cat", "mkfifo", "pkill");
 * // => validatePathResult.success
 *
 * @example
 * _validatePath("non-exist-directory", "bash", "bashdb", "cat", "mkfifo", "pkill");
 * // => validatePathResult.notExistCwd
 *
 * @example
 * _validatePath("./", "invalid-bash-path", "bashdb", "cat", "mkfifo", "pkill");
 * // => validatePathResult.notFoundBash
 *
 * @example
 * _validatePath("./", "bash", "invalid-bashdb-path", "cat", "mkfifo", "pkill");
 * // => validatePathResult.notFoundBashdb
 *
 * @example
 * _validatePath("./", "bash", "bashdb", "invalid-cat-path", "mkfifo", "pkill");
 * // => validatePathResult.notFoundCat
 *
 * @example
 * _validatePath("./", "bash", "bashdb", "cat", "invalid-mkfifo-path", "pkill");
 * // => validatePathResult.notFoundMkfifo
 *
 * @example
 * _validatePath("./", "bash", "bashdb", "cat", "mkfifo", "invalid-pkill-path");
 * // => validatePathResult.notFoundPkill
 *
 * @example
 * _validatePath("./", "bash", "bashdb", "cat", "mkfifo", "invalid-pkill-path");
 * // => validatePathResult.notFoundPkill
 *
 * @example
 * _validatePath("invalid-path", "invalid-path", "invalid-path", "invalid-path", "invalid-path", "invalid-path");
 * // => validatePathResult.notFoundBash
 *
 * @example
 * _validatePath("./", "bash", "; sleep 2", "", "", "");
 * // => validatePathResult.timeout
 */
function _validatePath(cwd, pathBash, pathBashdb, pathCat, pathMkfifo, pathPkill) {
    const vpr = validatePathResult;
    const argv = ["-c",
        `type ${pathBashdb} || exit ${vpr.notFoundBashdb};` +
            `type ${pathCat} || exit ${vpr.notFoundCat};` +
            `type ${pathMkfifo} || exit ${vpr.notFoundMkfifo};` +
            `type ${pathPkill} || exit ${vpr.notFoundPkill};` +
            `test -d ${cwd} || exit ${vpr.notExistCwd};` +
            ""
    ];
    const proc = child_process_1.spawnSync(pathBash, argv, { timeout: 1000 });
    if (proc.error !== undefined) {
        // @ts-ignore Property 'code' does not exist on type 'Error'.
        if (proc.error.code === "ENOENT") {
            return vpr.notFoundBash;
        }
        // @ts-ignore Property 'code' does not exist on type 'Error'.
        if (proc.error.code === "ETIMEDOUT") {
            return vpr.timeout;
        }
        return vpr.unknown;
    }
    if (proc.status === vpr.notExistCwd) {
        return vpr.notExistCwd;
    }
    return proc.status;
}
/**
 * @returns "" on success, non-empty error message on failure.
 * @example
 * validatePath("./", "bash", "bashdb", "cat", "mkfifo", "pkill");
 * // => ""
 * @example
 * validatePath("non-exist-directory", "bash", "bashdb", "cat", "mkfifo", "pkill");
 * // => "Error: cwd (non-exist-directory) does not exist."
 */
function validatePath(cwd, pathBash, pathBashdb, pathCat, pathMkfifo, pathPkill) {
    const askReport = "If it is reproducible, please report it to " +
        "https://github.com/rogalmic/vscode-bash-debug/issues";
    const rc = _validatePath(cwd, pathBash, pathBashdb, pathCat, pathMkfifo, pathPkill);
    switch (rc) {
        case validatePathResult.success: {
            return "";
        }
        case validatePathResult.notExistCwd: {
            return `Error: cwd (${cwd}) does not exist.`;
        }
        case validatePathResult.notFoundBash: {
            return `Error: bash not found. (pathBash: ${pathBash})`;
        }
        case validatePathResult.notFoundBashdb: {
            return `Error: bashdb not found. (pathBashdb: ${pathBashdb})`;
        }
        case validatePathResult.notFoundCat: {
            return `Error: cat not found. (pathCat: ${pathCat})`;
        }
        case validatePathResult.notFoundMkfifo: {
            return `Error: mkfifo not found. (pathMkfifo: ${pathMkfifo})`;
        }
        case validatePathResult.notFoundPkill: {
            return `Error: pkill not found. (pathPkill: ${pathPkill})`;
        }
        case validatePathResult.timeout: {
            return "Error: BUG: timeout " +
                "while validating environment. " + askReport;
        }
        case validatePathResult.unknown: {
            return "Error: BUG: unknown error ocurred " +
                "while validating environment. " + askReport;
        }
    }
    return "Error: BUG: reached to unreachable code " +
        "while validating environment. " + askReport;
}
exports.validatePath = validatePath;
//# sourceMappingURL=bashRuntime.js.map