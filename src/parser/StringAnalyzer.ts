import { isAlpha, isDecimalMark, isDigit, isDigitDot, isHexDigit, isWhitespace } from "../utils/helper";

export class StringAnalyzer {

    /** It holds the main string this class is working on */
    private _text: string = '';

    /** based on theory of computation head is the poiner to the current character */
    private _head: number = 0;

    /** this is responsible for going further on string virtually and if every thing is acceptable it represent that piece of string between _head and _scout */
    private _scout: number = 0;

    /**
     * @param text this is the text you want to analyze in this class
     */
    constructor(text: string) {
        this._text = text;
    }

    /** this represents the location (index) of where the class is ready to analyse */
    get head(): number { return this._head; }

    /** this represents the location (index) of where the class has searching before moving (scouting) to analyse */
    get scout(): number { return this._scout; }

    /** this sets the text the class is going to work on and resets the head and scout index to 0
     * @param {string} text the text that you want to analyze
     * @returns {StringAnalyzer} 
     */
    setText(text: string): this {
        this._text = text;
        this.resetAll();
        return this;
    }

    /** this method reset all the states of the class to the first step*/
    resetAll() { this.resetHead(); this.resetScout(); return this; }

    /** this just reset the head location to the first of the string being analyzed */
    resetHead(): this { this._head = 0; return this; }

    /** this just reset the scout location to the current `head`'s location */
    resetScout(): this { this._scout = this._head; return this; }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * It changes the location (index) of the head on the string forward by an amount of `step`
     * @param {number} step the number of steps to move `head`
     */
    headForward(step: number = 1): this { this._head += step; this.resetScout(); return this; }

    /**
     * It moves the location (index) of the head on the string backward by an amount of `step`
     * @param {number} [step=1] the number of steps to move `head`
     */
    headBackward(step: number = 1): this { this._head -= step; this.resetScout(); return this }

    /**
     * It moves the location (index) of the `scout` on the string forward by an amount of `step`
     * @param {number} [step=1] the number of steps to move `scout`
     */
    scoutForward(step: number = 1): this { this._scout += step; return this; }

    /**
     * It moves the location (index) of the `scout` on the string backward by an amount of `step`
     * @param {number} [step=1] the number of steps to move `scout`
     */
    scoutbackward(step: number = 1): this { this._scout -= step; return this; }

    /**
     * it skips the characters entered as arguments and change the location of the head
     * @param {Array<string>} ignoredCharList a list of characters to be ignored
     */
    skipIgnoredCharacters(ignoredCharList: Array<string> = [' ', '\t', '\n']) {
        // skip over ignored characters:
        while (true) {
            if (ignoredCharList.includes(this.charByHead()))
                this.headForward();
            else break;
        }
    }

    skipComments() {

    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * View upto `length` characters of the expression starting at the head character.
     * @param {number} [length=1] Number of characters to view
     * @returns {string}
     */
    chunkFromHead(length: number = 1): string {
        if (!(this.remainedCharByHead() >= length)) throw new Error("StringAnalyzer.getChunkByHead | there is a problem");
        return this._text.substring(this._head, this._head + length)
    }

    /**
     * View upto `length` characters of the expression starting at the scout character.
     * @param {number} [length=1] Number of characters to view
     * @returns {string}
     */
    chunkfromScout(length: number = 1): string {
        if (!(this.remainedCharByHead() >= length)) throw new Error("StringAnalyzer.getChunkByscout | there is a problem");
        return this._text.substring(this._scout, this._scout + length)
    }

    chunkHeadToScout(): string {
        if (!(this.remainedCharByHead() >= this._scout - this._head)) throw new Error("StringAnalyzer.chunkHeadToScout | there is a problem");
        return this._text.substring(this._head, this._scout)
    }

    /** 
     * this method helps to get a character with an `offset` distance from head
     * @param {number} offset the distance from head. it can be positive or negative
     * @returns {string} a char
     */
    charByHead(offset: number = 0): string {
        return this._text.substring(this._head + offset, this._head + offset + 1)
    }

    /** 
     * this method returns a character with an `offset` distance from head
     * @param {number} offset the distance from `scout`. it can be positive or negative
     * @returns {string} a char
     */
    charByScout(offset: number = 0): string {
        return this._text.substring(this._scout + offset, this._scout + offset + 1)
    }

    /**
     * it checks if the current character is the given character or not
     * @param {string} char the character to be checked
     * @returns {boolean} boolean
     */
    headCharIs(char: string, offset: number = 0): boolean {
        return this.charByHead(offset) === char;
    }

    /**
     * it checks if the character with an offset from scout location (index) is the given character or not
     * @param {string} char the character to be checked
     * @param {number} offset offset from the scout location
     * @returns {boolean} boolean
     */
    scoutCharIs(char: string, offset: number = 0): boolean {
        return this.charByScout(offset) === char;
    }

    /**
     * this method return the length of remained character from `head` location (index) to the end of the string if returns 0 means it is in the end of the string
     * @returns {number} return 0 or positive number
     */
    remainedCharByHead(): number {
        return Math.max(0, this._text.length - this._head);
    }

    /**
     * this method return the length of remained character from `scout` location (index) to the end of the string if returns 0 means it is in the end of the string
     * @returns {number} return 0 or positive number
     */
    remainedCharByScout(): number {
        return Math.max(0, this._text.length - this._scout);
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////

    isDigitDot(offset: number = 0): boolean {
        return isDigitDot(this.charByScout(offset));
    }

    isDigit(offset: number = 0): boolean {
        return isDigit(this.charByScout(offset));
    }

    isWhitespace(offset: number = 0) {
        return isWhitespace(this.charByScout(offset));
    }

    isDecimalMark(offset: number = 0): boolean {
        return isDecimalMark(this.charByScout(offset), this.charByScout(offset + 1));
    }

    isHexDigit(offset: number = 0): boolean {
        return isHexDigit(this.charByScout(offset));
    }

    isAlpha(offset: number = 0) {
        return isAlpha(this.charByScout(offset), this.charByScout(offset - 1), this.charByScout(offset + 1));
    }

    /**
     * it checks if there is any more character to checks. otherwise this means that it is at the end
     * @returns {boolean} true if it is the end 
     */
    isEndOfText(offset: number = 0): boolean { return this.headCharIs('', offset); }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * itnot only extracs the characters in the text from the location of `head` to the location of `scout` 
     * it also changes the `head` location and set to the `scout` location
     * @param {string} chars returns extracted charcter
     */
    tokenizeHeadToScout(): string {
        const result = this.chunkHeadToScout();
        this._head = this._scout;
        return result;
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * it generates a text with pointer under a specific location look at the below example
     * generated text:
     * ```
     *      ---------------------------
     *      24 + 12 ** 32 / 4
     *               ^
     *      ---------------------------
     * ```
     * @param {number} index location to start '^' being placed
     * @param {number} lengthOfPointers number of '^' char which are placed under the text
     * @returns {string} returns a text with pointer '^^^' under the location in index
     */
    generatePointer(index: number, lengthOfPointers: number = 1): string {
        const result = [];
        result.push(Array(this._text.length).fill("-").join());
        result.push(this._text);
        let signText = "";
        for (let i = 0; i < index; i++) signText += " ";
        for (let i = 0; i < lengthOfPointers; i++) signText += "^";
        result.push(signText);
        result.push(Array(this._text.length).fill("-").join());
        return result.join('\n');
    }


    generatePonterOnScout(): string {
        return this.generatePointer(this._scout)
    }
    generatePonterOnHead(): string {
        return this.generatePointer(this._head)
    }
}