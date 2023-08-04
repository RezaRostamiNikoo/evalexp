import { DELIMITERS } from "../constants";
import { isDecimalMark, isDigit, isDigitDot, isWhitespace } from "./helper";

export class StringChars {
    private expression: string = '';
    private index: number = 0;

    constructor(expression: string) {
        this.expression = expression;
    }

    getExpression() { return this.expression; }
    getIndex() { return this.index; }


    /**
     * View upto `length` characters of the expression starting at the current character.
     *
     * @param {Object} state
     * @param {number} [length=1] Number of characters to view
     * @returns {string}
     * @private
     */
    currentString(length: number = 1): string {
        return this.expression.substring(this.index, this.index + length)
    }

    /**
     * View the current character. Returns '' if end of expression is reached.
     *
     * @param {Object} state
     * @returns {string}
     * @private
     */
    currentCharacter(): string {
        return this.currentString(1)
    }

    get current(): string { return this.currentString(1); }
    /**
     * Get the next character from the expression.
     * The character is stored into the char c. If the end of the expression is
     * reached, the function puts an empty string in c.
     * @private
     */
    incrementIndex(step: number = 1) {
        this.index += step;
        return this;
    }

    /**
     * Preview the previous character from the expression.
     * @return {string} cNext
     * @private
     */
    prev(step: number = 1): string {
        return this.expression.charAt(this.index - step)
    }

    /**
     * Preview the next character from the expression.
     * @return {string} cNext
     * @private
     */
    next(step: number = 1): string {
        return this.expression.charAt(this.index + step)
    }

    /**
     * it checks if the current character is the given character or not
     * @param {string} char the character to be checked
     * @returns {boolean} boolean
     */
    isCurrent(char: string): boolean {
        return this.currentCharacter() === char;
    }

    isNext(char: string, step: number = 1): boolean {
        return this.next(step) === char;
    }
    isPrev(char: string, step: number = 1): boolean {
        return this.prev(step) === char;
    }

    isDelimiter(step: number = 1): boolean {
        const str = this.currentString(step);
        if (str.length === step)
            return DELIMITERS[str];
        return false;
    }

    isDigitDot(step: number = 1): boolean {
        return isDigitDot(this.currentString(step));
    }
    isDigit(step: number = 1): boolean {
        return isDigit(this.currentString(step));
    }

    isWhitespace(nestingLevel: number) {
        return isWhitespace(this.current, nestingLevel);
    }

    isDecimalMark(): boolean {
        return isDecimalMark(this.current, this.next());
    }

}