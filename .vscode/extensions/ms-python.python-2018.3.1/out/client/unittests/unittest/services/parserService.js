"use strict";
// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT License.
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __param = (this && this.__param) || function (paramIndex, decorator) {
    return function (target, key) { decorator(target, key, paramIndex); }
};
Object.defineProperty(exports, "__esModule", { value: true });
const inversify_1 = require("inversify");
const path = require("path");
const types_1 = require("../../common/types");
let TestsParser = class TestsParser {
    constructor(testsHelper) {
        this.testsHelper = testsHelper;
    }
    parse(content, options) {
        const testIds = this.getTestIds(content);
        let testsDirectory = options.cwd;
        if (options.startDirectory.length > 1) {
            testsDirectory = path.isAbsolute(options.startDirectory) ? options.startDirectory : path.resolve(options.cwd, options.startDirectory);
        }
        return this.parseTestIds(testsDirectory, testIds);
    }
    getTestIds(content) {
        let startedCollecting = false;
        return content.split(/\r?\n/g)
            .map((line, index) => {
            if (!startedCollecting) {
                if (line === 'start') {
                    startedCollecting = true;
                }
                return '';
            }
            return line.trim();
        })
            .filter(line => line.length > 0);
    }
    parseTestIds(rootDirectory, testIds) {
        const testFiles = [];
        testIds.forEach(testId => {
            this.addTestId(rootDirectory, testId, testFiles);
        });
        return this.testsHelper.flattenTestFiles(testFiles);
    }
    addTestId(rootDirectory, testId, testFiles) {
        const testIdParts = testId.split('.');
        // We must have a file, class and function name
        if (testIdParts.length <= 2) {
            return null;
        }
        const paths = testIdParts.slice(0, testIdParts.length - 2);
        const filePath = `${path.join(rootDirectory, ...paths)}.py`;
        const functionName = testIdParts.pop();
        const className = testIdParts.pop();
        // Check if we already have this test file
        let testFile = testFiles.find(test => test.fullPath === filePath);
        if (!testFile) {
            testFile = {
                name: path.basename(filePath),
                fullPath: filePath,
                // tslint:disable-next-line:prefer-type-cast
                functions: [],
                // tslint:disable-next-line:prefer-type-cast
                suites: [],
                nameToRun: `${className}.${functionName}`,
                xmlName: '',
                status: types_1.TestStatus.Idle,
                time: 0
            };
            testFiles.push(testFile);
        }
        // Check if we already have this test file
        const classNameToRun = className;
        let testSuite = testFile.suites.find(cls => cls.nameToRun === classNameToRun);
        if (!testSuite) {
            testSuite = {
                name: className,
                // tslint:disable-next-line:prefer-type-cast
                functions: [],
                // tslint:disable-next-line:prefer-type-cast
                suites: [],
                isUnitTest: true,
                isInstance: false,
                nameToRun: classNameToRun,
                xmlName: '',
                status: types_1.TestStatus.Idle,
                time: 0
            };
            testFile.suites.push(testSuite);
        }
        const testFunction = {
            name: functionName,
            nameToRun: testId,
            status: types_1.TestStatus.Idle,
            time: 0
        };
        testSuite.functions.push(testFunction);
    }
};
TestsParser = __decorate([
    inversify_1.injectable(),
    __param(0, inversify_1.inject(types_1.ITestsHelper))
], TestsParser);
exports.TestsParser = TestsParser;
//# sourceMappingURL=parserService.js.map