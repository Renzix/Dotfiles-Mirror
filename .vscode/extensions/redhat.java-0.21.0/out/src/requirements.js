'use strict';
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_1 = require("vscode");
const cp = require("child_process");
const path = require("path");
const pathExists = require("path-exists");
const expandHomeDir = require("expand-home-dir");
const findJavaHome = require("find-java-home");
const isWindows = process.platform.indexOf('win') === 0;
const JAVAC_FILENAME = 'javac' + (isWindows ? '.exe' : '');
/**
 * Resolves the requirements needed to run the extension.
 * Returns a promise that will resolve to a RequirementsData if
 * all requirements are resolved, it will reject with ErrorData if
 * if any of the requirements fails to resolve.
 *
 */
function resolveRequirements() {
    return __awaiter(this, void 0, void 0, function* () {
        let java_home = yield checkJavaRuntime();
        let javaVersion = yield checkJavaVersion(java_home);
        return Promise.resolve({ 'java_home': java_home, 'java_version': javaVersion });
    });
}
exports.resolveRequirements = resolveRequirements;
function checkJavaRuntime() {
    return new Promise((resolve, reject) => {
        let source;
        let javaHome = readJavaConfig();
        if (javaHome) {
            source = 'The java.home variable defined in VS Code settings';
        }
        else {
            javaHome = process.env['JDK_HOME'];
            if (javaHome) {
                source = 'The JDK_HOME environment variable';
            }
            else {
                javaHome = process.env['JAVA_HOME'];
                source = 'The JAVA_HOME environment variable';
            }
        }
        if (javaHome) {
            javaHome = expandHomeDir(javaHome);
            if (!pathExists.sync(javaHome)) {
                openJDKDownload(reject, source + ' points to a missing folder');
            }
            if (!pathExists.sync(path.resolve(javaHome, 'bin', JAVAC_FILENAME))) {
                openJDKDownload(reject, source + ' does not point to a JDK.');
            }
            return resolve(javaHome);
        }
        //No settings, let's try to detect as last resort.
        findJavaHome(function (err, home) {
            if (err) {
                openJDKDownload(reject, 'Java runtime could not be located');
            }
            else {
                resolve(home);
            }
        });
    });
}
function readJavaConfig() {
    const config = vscode_1.workspace.getConfiguration();
    return config.get('java.home', null);
}
function checkJavaVersion(java_home) {
    return new Promise((resolve, reject) => {
        cp.execFile(java_home + '/bin/java', ['-version'], {}, (error, stdout, stderr) => {
            let javaVersion = parseMajorVersion(stderr);
            if (javaVersion < 8) {
                openJDKDownload(reject, 'Java 8 or more recent is required to run. Please download and install a recent JDK');
            }
            else {
                resolve(javaVersion);
            }
        });
    });
}
function parseMajorVersion(content) {
    let regexp = /version "(.*)"/g;
    let match = regexp.exec(content);
    if (!match) {
        return 0;
    }
    let version = match[1];
    //Ignore '1.' prefix for legacy Java versions
    if (version.startsWith('1.')) {
        version = version.substring(2);
    }
    //look into the interesting bits now
    regexp = /\d+/g;
    match = regexp.exec(version);
    let javaVersion = 0;
    if (match) {
        javaVersion = parseInt(match[0]);
    }
    return javaVersion;
}
exports.parseMajorVersion = parseMajorVersion;
function openJDKDownload(reject, cause) {
    let jdkUrl = 'http://developers.redhat.com/products/openjdk/overview/';
    if (process.platform === 'darwin') {
        jdkUrl = 'http://www.oracle.com/technetwork/java/javase/downloads/index.html';
    }
    reject({
        message: cause,
        label: 'Get Java Development Kit',
        openUrl: vscode_1.Uri.parse(jdkUrl),
        replaceClose: false
    });
}
//# sourceMappingURL=requirements.js.map