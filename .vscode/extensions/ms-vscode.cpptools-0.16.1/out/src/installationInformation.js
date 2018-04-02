Object.defineProperty(exports, "__esModule", { value: true });
class InstallationInformation {
    constructor() {
        this.hasError = false;
        this.telemetryProperties = {};
    }
}
exports.InstallationInformation = InstallationInformation;
let installBlob;
function initializeInstallationInformation() {
    installBlob = new InstallationInformation();
}
exports.initializeInstallationInformation = initializeInstallationInformation;
function getInstallationInformationInstance() {
    return installBlob;
}
exports.getInstallationInformationInstance = getInstallationInformationInstance;
function setInstallationStage(stage) {
    if (!installBlob) {
        initializeInstallationInformation();
    }
    installBlob.stage = stage;
}
exports.setInstallationStage = setInstallationStage;
//# sourceMappingURL=installationInformation.js.map