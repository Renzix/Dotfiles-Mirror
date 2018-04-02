"use strict";
// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT License.
Object.defineProperty(exports, "__esModule", { value: true });
const braceCounter_1 = require("../language/braceCounter");
const textBuilder_1 = require("../language/textBuilder");
const textRangeCollection_1 = require("../language/textRangeCollection");
const tokenizer_1 = require("../language/tokenizer");
const types_1 = require("../language/types");
class LineFormatter {
    constructor() {
        this.builder = new textBuilder_1.TextBuilder();
        this.tokens = new textRangeCollection_1.TextRangeCollection([]);
        this.braceCounter = new braceCounter_1.BraceCounter();
        this.text = '';
    }
    // tslint:disable-next-line:cyclomatic-complexity
    formatLine(text) {
        this.tokens = new tokenizer_1.Tokenizer().tokenize(text);
        this.text = text;
        this.builder = new textBuilder_1.TextBuilder();
        this.braceCounter = new braceCounter_1.BraceCounter();
        if (this.tokens.count === 0) {
            return this.text;
        }
        const ws = this.text.substr(0, this.tokens.getItemAt(0).start);
        if (ws.length > 0) {
            this.builder.append(ws); // Preserve leading indentation.
        }
        for (let i = 0; i < this.tokens.count; i += 1) {
            const t = this.tokens.getItemAt(i);
            const prev = i > 0 ? this.tokens.getItemAt(i - 1) : undefined;
            const next = i < this.tokens.count - 1 ? this.tokens.getItemAt(i + 1) : undefined;
            switch (t.type) {
                case types_1.TokenType.Operator:
                    this.handleOperator(i);
                    break;
                case types_1.TokenType.Comma:
                    this.builder.append(',');
                    if (next && !this.isCloseBraceType(next.type)) {
                        this.builder.softAppendSpace();
                    }
                    break;
                case types_1.TokenType.Identifier:
                    if (prev && !this.isOpenBraceType(prev.type) && prev.type !== types_1.TokenType.Colon && prev.type !== types_1.TokenType.Operator) {
                        this.builder.softAppendSpace();
                    }
                    this.builder.append(this.text.substring(t.start, t.end));
                    break;
                case types_1.TokenType.Colon:
                    // x: 1 if not in slice, x[1:y] if inside the slice.
                    this.builder.append(':');
                    if (!this.braceCounter.isOpened(types_1.TokenType.OpenBracket) && (next && next.type !== types_1.TokenType.Colon)) {
                        // Not inside opened [[ ... ] sequence.
                        this.builder.softAppendSpace();
                    }
                    break;
                case types_1.TokenType.Comment:
                    // Add space before in-line comment.
                    if (prev) {
                        this.builder.softAppendSpace();
                    }
                    this.builder.append(this.text.substring(t.start, t.end));
                    break;
                case types_1.TokenType.Semicolon:
                    this.builder.append(';');
                    break;
                default:
                    this.handleOther(t, i);
                    break;
            }
        }
        return this.builder.getText();
    }
    handleOperator(index) {
        const t = this.tokens.getItemAt(index);
        if (t.length === 1) {
            const opCode = this.text.charCodeAt(t.start);
            switch (opCode) {
                case 61 /* Equal */:
                    if (this.handleEqual(t, index)) {
                        return;
                    }
                    break;
                case 46 /* Period */:
                    this.builder.append('.');
                    return;
                case 64 /* At */:
                    this.builder.append('@');
                    return;
                default:
                    break;
            }
        }
        this.builder.softAppendSpace();
        this.builder.append(this.text.substring(t.start, t.end));
        this.builder.softAppendSpace();
    }
    handleEqual(t, index) {
        if (this.isMultipleStatements(index) && !this.braceCounter.isOpened(types_1.TokenType.OpenBrace)) {
            return false; // x = 1; x, y = y, x
        }
        // Check if this is = in function arguments. If so, do not add spaces around it.
        if (this.isEqualsInsideArguments(index)) {
            this.builder.append('=');
            return true;
        }
        return false;
    }
    handleOther(t, index) {
        if (this.isBraceType(t.type)) {
            this.braceCounter.countBrace(t);
            this.builder.append(this.text.substring(t.start, t.end));
            return;
        }
        if (this.isEqualsInsideArguments(index - 1)) {
            // Don't add space around = inside function arguments.
            this.builder.append(this.text.substring(t.start, t.end));
            return;
        }
        if (index > 0) {
            const prev = this.tokens.getItemAt(index - 1);
            if (this.isOpenBraceType(prev.type) || prev.type === types_1.TokenType.Colon) {
                // Don't insert space after (, [ or { .
                this.builder.append(this.text.substring(t.start, t.end));
                return;
            }
        }
        // In general, keep tokens separated.
        this.builder.softAppendSpace();
        this.builder.append(this.text.substring(t.start, t.end));
    }
    isEqualsInsideArguments(index) {
        if (index < 1) {
            return false;
        }
        const prev = this.tokens.getItemAt(index - 1);
        if (prev.type === types_1.TokenType.Identifier) {
            if (index >= 2) {
                // (x=1 or ,x=1
                const prevPrev = this.tokens.getItemAt(index - 2);
                return prevPrev.type === types_1.TokenType.Comma || prevPrev.type === types_1.TokenType.OpenBrace;
            }
            else if (index < this.tokens.count - 2) {
                const next = this.tokens.getItemAt(index + 1);
                const nextNext = this.tokens.getItemAt(index + 2);
                // x=1, or x=1)
                if (this.isValueType(next.type)) {
                    return nextNext.type === types_1.TokenType.Comma || nextNext.type === types_1.TokenType.CloseBrace;
                }
            }
        }
        return false;
    }
    isOpenBraceType(type) {
        return type === types_1.TokenType.OpenBrace || type === types_1.TokenType.OpenBracket || type === types_1.TokenType.OpenCurly;
    }
    isCloseBraceType(type) {
        return type === types_1.TokenType.CloseBrace || type === types_1.TokenType.CloseBracket || type === types_1.TokenType.CloseCurly;
    }
    isBraceType(type) {
        return this.isOpenBraceType(type) || this.isCloseBraceType(type);
    }
    isValueType(type) {
        return type === types_1.TokenType.Identifier || type === types_1.TokenType.Unknown ||
            type === types_1.TokenType.Number || type === types_1.TokenType.String;
    }
    isMultipleStatements(index) {
        for (let i = index; i >= 0; i -= 1) {
            if (this.tokens.getItemAt(i).type === types_1.TokenType.Semicolon) {
                return true;
            }
        }
        return false;
    }
}
exports.LineFormatter = LineFormatter;
//# sourceMappingURL=lineFormatter.js.map