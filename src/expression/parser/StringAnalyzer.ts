import { DELIMITERS } from "./constants";
import { isAlpha, isDecimalMark, isDigit, isDigitDot, isHexDigit, isWhitespace } from "../../utils/helper";

export class StringAnalyzer {

    /** It holds the main string this class is working on */
    private _text: string = '';


    ////////////////////////////////////
    /// States

    /** based on theory of computation head is the poiner to the current character */
    private _head: number = 0;

    /** this is responsible for going further on string virtually and if every thing is acceptable it represent that piece of string between _head and _scout */
    private _scout: number = 0;

    ////////////////////////////////////

    /**
     * @param text this is the text you want to analyze in this class
     */
    constructor(text: string) {
        this._text = text;
    }

    /** this represents the location (index) of where the class is ready to analyse */
    get head(): number { return this._head; }

    /** this represents the location (index) of where the class has searching before moving (scouting) to analyse */
    get Scout(): number { return this._scout; }

    /** this reset the text the class is going to work on
     * @param {string} text the text that you want to analyze
     * @returns {StringAnalyzer} 
     */
    resetText(text: string): this {
        this._text = text;
        this.resetAll();
        return this;
    }

    /** this method reset all the states of the class to the first step*/
    resetAll() { this.resetHead(); this.resetScout(); return this; }

    /** this just reset the head location to the first of the string being analyzed */
    resetHead(): this { this._head = 0; return this; }

    /** this just reset the scout location to the first of the string being analyzed */
    resetScout(): this { this._scout = 0; return this; }

    /**
     * It changes the location (index) of the head on the string forward by an amount of `step`
     * @param {number} step the number of steps to move `head`
     */
    moveForward(step: number = 1) {
        this._head += step;
    }

    /**
     * It moves the location (index) of the head on the string backward by an amount of `step`
     * @param {number} [step=1] the number of steps to move `head`
     */
    moveBackward(step: number = 1) {
        this._head -= step;
    }

    /**
     * It moves the location (index) of the `scout` on the string forward by an amount of `step`
     * @param {number} [step=1] the number of steps to move `scout`
     */
    scoutForward(step: number = 1) {
        this._scout += step;
    }

    /**
     * It moves the location (index) of the `scout` on the string backward by an amount of `step`
     * @param {number} [step=1] the number of steps to move `scout`
     */
    scoutbackward(step: number = 1) {
        this._scout += step;
    }

    /**
     * View upto `length` characters of the expression starting at the current character.
     * @param {number} [length=1] Number of characters to view
     * @returns {string}
     */
    getChunk(length: number = 1): string {
        return this._text.substring(this._head, this._head + length)
    }

    /** 
     * this method helps to get a character with an `offset` distance from head
     * @param {number} offset the distance from head. it can be positive or negative
     * @returns {string} a char
     */
    getCharByHead(offset: number = 0): string {
        return this._text.substring(this._head + offset, this._head + offset + 1)
    }

    /** 
     * this method returns a character with an `offset` distance from head
     * @param {number} offset the distance from `scout`. it can be positive or negative
     * @returns {string} a char
     */
    getCharByScout(offset: number = 0): string {
        return this._text.substring(this._scout, this._scout + offset)
    }

    /**
     * View the current character. Returns '' if end of expression is reached.
     * @returns {string}
     */
    current(): string { return this.getChunk(1); }

    /**
     * Preview the previous character from the expression.
     * @param {number} step number of steps backward
     * @return {string} return the previous characters
     */
    prev(step: number = 1): string {
        return this._text.charAt(this._head - step)
    }

    /**
     * Preview the next character from the expression.
     * @param {number} step number of steps forward
     * @return {string} return string/one character
     */
    next(step: number = 1): string {
        return this._text.charAt(this._head + step)
    }

    /**
     * it checks if the current character is the given character or not
     * @param {string} char the character to be checked
     * @returns {boolean} boolean
     */
    currentIs(char: string): boolean {
        return this.current() === char;
    }

    nextIs(char: string, step: number = 1): boolean {
        return this.next(step) === char;
    }

    prevIs(char: string, step: number = 1): boolean {
        return this.prev(step) === char;
    }

    isDigitDot(): boolean {
        return isDigitDot(this.current());
    }

    isDigit(offset: number = 0): boolean {
        return isDigit(this.next(offset));
    }

    isWhitespace() {
        return isWhitespace(this.current());
    }

    isDecimalMark(): boolean {
        return isDecimalMark(this.current(), this.next());
    }

    isHexDigit(): boolean {
        return isHexDigit(this.current());
    }

    isAlpha() {
        return isAlpha(this.current(), this.prev(), this.next());
    }


    /**
     * it skips the characters entered as arguments and change the location of the head
     * @param {Array<string>} chars a list of characters to be ignored
     */
    skipIgnoredCharacters(chars: Array<string> = [' ', '\t', '\n']) {
        // skip over ignored characters:
        while (true) {
            if (chars.includes(this.current()))
                this.moveForward();
            else break;
        }
    }

    skipComments() {
        // while (this.getChunk(2))
            // if ()
    }
    /**
     * it checks if there is any more character to checks. otherwise this means that it is at the end
     * @returns {boolean} true if it is the end 
     */
    isEnd(): boolean { return this.currentIs(undefined); }

    /**
     * it checks if the string based on the number of charaters is kind of delimiters
     * @param {boolean} chars number of character to be extracted from text and checked against delimiers
     * @returns {boolean} returns boolean
     */
    isDelimiter(chars: number) {
        const str = this.getChunk(chars);
        if (str.length === chars)
            return DELIMITERS[str];
        return false;
    }

    /**
     * itnot only extracs the characters in the text from the location of head forward 
     * it also changes the head location based on the number of character it extracts
     * @param {string} chars returns extracted charcter
     */
    extractChar(chars: number = 1): string {
        const result = this.getChunk(chars);
        this.moveForward(chars);
        return result;
    }


    getErrorOnHead(chars: number, head: number): string {
        const result = [];
        result.push(this._text);
        let signText = "";
        for (let i = 0; i < head; i++) signText += " ";
        for (let i = 0; i < chars; i++) signText += "^";
        result.push(signText);
        return result.join('\n');
    }


}