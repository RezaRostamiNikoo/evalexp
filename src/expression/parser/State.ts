
import { hasOwnProperty } from "../../utils/object";
import { CONSTANTS, DELIMITERS, NAMED_DELIMITERS, NUMERIC_CONSTANTS, TOKENTYPE } from "../constants"
import { StringChars } from "./StringChars";
import { Token } from "./Token";

export class State {
    private state = {
        extraNodes: {}, // current extra nodes, must be careful not to mutate
        chars: new StringChars(''), // current expression
        comment: '', // last parsed comment
        token: new Token('', TOKENTYPE.NULL), // current token
        nestingLevel: 0, // level of nesting inside parameters, used to ignore newline characters
        conditionalLevel: null, // when a conditional is being parsed, the level of the conditional is stored here
    }
    get extraNodes() { return this.state.extraNodes; }
    get expression() { return this.state.chars.getExpression(); }
    get chars() { return this.state.chars; }
    get comment() { return this.state.comment; }
    get index() { return this.state.chars.getIndex(); }
    get token() { return this.state.token; }
    get tokenValue() { return this.state.token.getValue(); }
    get tokenType() { return this.state.token.getType(); }
    get nestingLevel() { return this.state.nestingLevel; }
    get conditionalLevel() { return this.state.conditionalLevel; }

    constructor(expression: string, extraNodes?: Object) {
        this.state.chars = new StringChars(expression);
        this.state.extraNodes = extraNodes;
    }


    // /**
    //  * it checks if the current token is the given token or not
    //  * @param {string} token the token to be checked
    //  * @returns {boolean} boolean
    //  */
    // isToken(token: string): boolean {
    //     return this.state.token !== token;
    // }



    // /**
    //  * it sets token type
    //  * @param {number} tokenType token type
    //  */
    // setTokenType(tokenType: number) {
    //     this.state.tokenType = tokenType;
    // }

    // /**
    //  * it sets token 
    //  * @param {string} token token 
    //  */
    // setToken(token: string) {
    //     this.state.token = token;
    // }

    // /**
    //  * it appends a token to the current token 
    //  * @param {string} token token 
    //  */
    // appendToken(token: string) {
    //     this.state.token += token;
    // }

    /**
    * it sets comments
    * @param {string} comment
    */
    setComment(comment: string) {
        this.state.comment = comment;
    }

    /**
    * it appends comments
    * @param {string} comment
    */
    appendComment(comment: string) {
        this.state.comment += comment;
    }

    set(token: string, type?: number) {
        this.token.set(token, type);
        this.chars.incrementIndex(token.length);
    }
    setCurrent(type?: number) {
        this.token.set(this.chars.current, type);
        this.chars.incrementIndex();
    }
    appendCurrent(type?: number) {
        this.token.append(this.chars.current, type);
        this.chars.incrementIndex();
    }

    /**
     * Get next token in the current string expr.
     * The token and token type are available as token and tokenType
     * @private
     */
    getToken(): string {
        this.token.set('', TOKENTYPE.NULL)
        this.setComment('');

        this.skipIgnoredCharacters();
        if (this.isEnd()) return this.tokenValue;
        if (this.isDelimiter()) return this.tokenValue;
        if (this.is_bi_oct_hex()) return this.tokenValue;
        if (this.isNumber()) return this.tokenValue;
        if (this.is_variable_function_namedOperator()) return this.tokenValue;

        // something unknown is found, wrong characters -> a syntax error
        this.token.setTypeTo_UNKNOWN();
        while (this.chars.currentCharacter() !== '') {
            this.appendCurrent();
        }
        // throw this.createSyntaxError('Syntax error in part "' + this.token + '"')
        throw new Error('Syntax error in part "' + this.tokenValue + '"')
    }

    ////////////////////////
    skipIgnoredCharacters() {
        // skip over ignored characters:
        while (true) {
            // TODO: you can add whatever you want to be ignored here
            // whitespace: space, tab, and newline when inside parameters
            if (this.chars.isWhitespace()) {
                // I changed thesource code from mathjs: so that it recognize \n as white space and 
                // the nesting level doesn't matter 
                this.chars.incrementIndex();
            } else {
                break
            }
        }
    }

    /**
     * it checks if there is any more character to checks. otherwise this means that it is at the end
     * @returns {boolean} true if it is the end 
     */
    isEnd(): boolean {
        // check for end of expression
        if (this.chars.isCurrent('')) {
            // token is still empty
            this.token.setTypeTo_DELIMITER();
            return true;
        }
        return false;
    }

    isDelimiter() {
        const maxLengthOfADelimiter = 4;
        for (let l = maxLengthOfADelimiter; l > 0; l--) {
            if (this.chars.isDelimiter(l)) {
                this.set(this.chars.currentString(l), TOKENTYPE.DELIMITER);
                return true;
            }
        }
        return false;
    }

    isNumber() {
        // check for a number
        if (this.chars.isDigitDot()) {
            this.token.setTypeTo_NUMBER();

            // get number, can have a single dot
            if (this.chars.isCurrent('.')) {
                this.appendCurrent();

                if (!this.chars.isDigit()) {
                    // this is no number, it is just a dot (can be dot notation)
                    this.token.setTypeTo_DELIMITER();
                    return false;
                }
            } else {
                this.appendAllDigits();
                if (this.chars.isDecimalMark()) this.appendCurrent();
            }

            this.appendAllDigits();
            // check for exponential notation like "2.3e-4", "1.23e50" or "2e+4"
            if (this.chars.isCurrent('E') || this.chars.isCurrent('e')) {
                if (this.chars.isDigit(1) || this.chars.isNext('-') || this.chars.isNext('+')) {
                    this.appendCurrent();

                    if (this.chars.isCurrent('+') || this.chars.isCurrent('-')) this.appendCurrent();

                    // Scientific notation MUST be followed by an exponent
                    if (!this.chars.isDigit()) {
                        // throw this.createSyntaxError('Digit expected, got "' + this.currentCharacter() + '"')
                        throw new Error('Digit expected, got "' + this.chars.current + '"')
                    }

                    this.appendAllDigits();

                    if (this.chars.isDecimalMark()) {
                        // throw this.createSyntaxError('Digit expected, got "' + this.currentCharacter() + '"')
                        throw new Error('Digit expected, got "' + this.chars.currentCharacter() + '"')
                    }
                } else if (this.chars.isNext('.')) {
                    this.chars.incrementIndex()
                    // throw this.createSyntaxError('Digit expected, got "' + this.currentCharacter() + '"')
                    throw new Error('Digit expected, got "' + this.chars.currentCharacter() + '"')
                }
            }

            return true;
        }

        return false;
    }

    is_bi_oct_hex(): boolean {
        // check for binary, octal, or hex
        const c2 = this.chars.currentString(2)
        if (c2 === '0b' || c2 === '0o' || c2 === '0x') {
            this.appendCurrent(TOKENTYPE.NUMBER);
            this.appendCurrent();
            this.appendAllHexDigits();

            if (this.chars.isCurrent('.')) {
                // this number has a radix point
                this.set('.');

                // get the digits after the radix
                this.appendAllHexDigits()

            } else if (this.chars.isCurrent('i')) {
                // this number has a word size suffix
                this.set('i');
                // get the word size
                this.appendAllDigits();

            }
            return true;
        }

        return false;
    }
    is_variable_function_namedOperator(): boolean {
        // check for variables, functions, named operators
        if (this.chars.isAlpha()) {
            while (this.chars.isAlpha() || this.chars.isDigit()) this.appendCurrent()

            if (hasOwnProperty(NAMED_DELIMITERS, this.tokenValue)) {
                this.token.setTypeTo_DELIMITER();
            } else {
                this.token.setTypeTo_SYMBOL();
            }

            return true;
        }
        return false;
    }

    /**
     * Get next token and skip newline tokens
     */
    getTokenSkipNewline() {
        do {
            this.getToken()
        }
        while (this.token.equal('\n')) // eslint-disable-line no-unmodified-loop-condition
    }

    /**
     * Open parameters.
     * New line characters will be ignored untilthis.closeParams() is called
     */
    openParams() {
        this.state.nestingLevel++
    }

    /**
     * Close parameters.
     * New line characters will no longer be ignored
     */
    closeParams() {
        this.state.nestingLevel--
    }

    appendAllDigits() {
        while (this.chars.isDigit()) this.appendCurrent();
    }
    appendAllHexDigits() {
        while (this.chars.isHexDigit()) this.appendCurrent();
    }
}

