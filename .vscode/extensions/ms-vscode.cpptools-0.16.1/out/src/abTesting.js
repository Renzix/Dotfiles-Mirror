'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const url = require("url");
const https = require("https");
const vscode = require("vscode");
const fs = require("fs");
const util = require("./common");
const Telemetry = require("./telemetry");
const userBucketMax = 100;
const userBucketString = "CPP.UserBucket";
function activate(context) {
    if (context.globalState.get(userBucketString, -1) === -1) {
        let bucket = Math.floor(Math.random() * userBucketMax) + 1;
        context.globalState.update(userBucketString, bucket);
    }
    setInterval(() => {
        downloadCpptoolsJsonPkg();
    }, 30 * 60 * 1000);
}
exports.activate = activate;
function downloadCpptoolsJson(urlString) {
    return new Promise((resolve, reject) => {
        let parsedUrl = url.parse(urlString);
        let request = https.request({
            host: parsedUrl.host,
            path: parsedUrl.path,
            agent: util.GetHttpsProxyAgent(),
            rejectUnauthorized: vscode.workspace.getConfiguration().get("http.proxyStrictSSL", true)
        }, (response) => {
            if (response.statusCode === 301 || response.statusCode === 302) {
                let redirectUrl;
                if (typeof response.headers.location === "string") {
                    redirectUrl = response.headers.location;
                }
                else {
                    redirectUrl = response.headers.location[0];
                }
                return resolve(downloadCpptoolsJson(redirectUrl));
            }
            if (response.statusCode !== 200) {
                return reject();
            }
            let downloadedBytes = 0;
            let cppToolsJsonFile = fs.createWriteStream(util.getExtensionFilePath("cpptools.json"));
            response.on('data', (data) => { downloadedBytes += data.length; });
            response.on('end', () => { cppToolsJsonFile.close(); });
            cppToolsJsonFile.on('close', () => { resolve(); });
            response.on('error', (error) => { reject(); });
            response.pipe(cppToolsJsonFile, { end: false });
        });
        request.on('error', (error) => { reject(); });
        request.end();
    });
}
function downloadCpptoolsJsonPkg() {
    let hasError = false;
    let telemetryProperties = {};
    return downloadCpptoolsJson("https://go.microsoft.com/fwlink/?linkid=852750")
        .catch((error) => {
        hasError = true;
    })
        .then(() => {
        telemetryProperties['success'] = (!hasError).toString();
        Telemetry.logDebuggerEvent("cpptoolsJsonDownload", telemetryProperties);
    });
}
exports.downloadCpptoolsJsonPkg = downloadCpptoolsJsonPkg;
function processCpptoolsJson(cpptoolsString) {
    let cpptoolsObject = JSON.parse(cpptoolsString);
    let intelliSenseEnginePercentage = cpptoolsObject.intelliSenseEngine_default_percentage;
    if (!util.packageJson.extensionFolderPath.includes(".vscode-insiders")) {
        let prevIntelliSenseEngineDefault = util.packageJson.contributes.configuration.properties["C_Cpp.intelliSenseEngine"].default;
        if (util.extensionContext.globalState.get(userBucketString, userBucketMax + 1) <= intelliSenseEnginePercentage) {
            util.packageJson.contributes.configuration.properties["C_Cpp.intelliSenseEngine"].default = "Default";
        }
        else {
            util.packageJson.contributes.configuration.properties["C_Cpp.intelliSenseEngine"].default = "Tag Parser";
        }
        if (prevIntelliSenseEngineDefault !== util.packageJson.contributes.configuration.properties["C_Cpp.intelliSenseEngine"].default) {
            return util.writeFileText(util.getPackageJsonPath(), util.getPackageJsonString());
        }
    }
}
exports.processCpptoolsJson = processCpptoolsJson;
//# sourceMappingURL=abTesting.js.map