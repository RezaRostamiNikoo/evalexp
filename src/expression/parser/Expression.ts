import { EmptyExpressionError } from "../errors";
import { Token } from "./Token";
import { DELIMITERS } from "./constants";
import { TokenType } from "./types";
import { StringAnalyzer } from "./StringAnalyzer";

export class Expression {
    private text: StringAnalyzer;
    private lastToken: Token;
    constructor(expression: string) {
        if (!expression) throw new EmptyExpressionError()
        this.text = new StringAnalyzer(expression);
    }

    /**
     * Get next token in the current string expr.
     * The token and token type are available as token and tokenType
     * @private
     */
    getNextToken(): Token {
        const result = this.calculateNextToken();
        if (this.lastToken && result.Value) {
            this.lastToken.Next = result;
            result.Prev = this.lastToken;
        }
        this.lastToken = result;
        return result;
    }

    private calculateNextToken(): Token {
        const result = new Token('', "NULL");
        this.text.skipIgnoredCharacters();
        // this.text.skipComments(); // TODO: this skip comments maybe it is important in the future
        if (this.text.isEndOfText()) return undefined;

        


        if (this.isDelimiter(result)) return result;
        if (this.is_bi_oct_hex(result)) return result;
        if (this.isNumber(result)) return result;
        if (this.is_variable_function_namedOperator(result)) return result;
        if (this.isEnd(result)) {
            result.head = this.text.head - result.Value.length;
            return result;
        }
        if (this.isDelimiter(result)) {
            result.head = this.text.head - result.Value.length;
            return result;
        }
        if (this.is_bi_oct_hex(result)) {
            result.head = this.text.head - result.Value.length;
            return result;
        }
        if (this.isNumber(result)) {
            result.head = this.text.head - result.Value.length;
            return result;
        }
        if (this.is_variable_function_namedOperator(result)) {
            result.head = this.text.head - result.Value.length;
            return result;
        }

        // something unknown is found, wrong characters -> a syntax error
        result.setType("UNKNOWN");
        while (!this.text.headCharIs('')) { this.appendCurrent(result); }
        return result;
    }

    isDelimiter(token: Token):TokenType {
        const maxLengthOfADelimiter = 4;
        for (let step = maxLengthOfADelimiter; step > 0; step--) {
            if (this.text.headToEndLength() >= step) {
                const str = this.text.chunkFromHead(step)
                if (DELIMITERS[str]) {
                    this.set(token, str, "DELIMITER");
                    return true;
                }
            }
        }
        return false;
    }

    is_bi_oct_hex(token: Token): TokenType {
        // check for binary, octal, or hex
        const c2 = this.text.chunkFromHead(2)
        if (c2 === '0b' || c2 === '0o' || c2 === '0x') {
            this.appendCurrent(token, "NUMBER");
            this.appendCurrent(token);
            this.appendAllHexDigits(token);

            if (this.text.headCharIs('.')) {
                // this number has a radix point
                this.set(token, '.');

                // get the digits after the radix
                this.appendAllHexDigits(token)

            } else if (this.text.headCharIs('i')) {
                // this number has a word size suffix
                this.set(token, 'i');
                // get the word size
                this.appendAllDigits(token);

            }

            return true;
        }

        return false;
    }


    set(token: Token, str: string, type?: TokenType) {
        token.set(this.text.extractChar(str.length), type);
    }
    setCurrent(token: Token, type?: TokenType) {
        token.set(this.text.extractChar(), type);
    }
    appendCurrent(token: Token, type?: TokenType) {
        token.append(this.text.extractChar(), type);
    }


    appendAllDigits(token: Token) {
        while (this.text.isDigit()) this.appendCurrent(token);
    }

    appendAllHexDigits(token: Token) {
        while (this.text.isHexDigit()) this.appendCurrent(token);
    }

    isNumber(token: Token): TokenType {
        // check for a number
        if (this.text.isDigitDot()) {
            token.setType("NUMBER");

            // get number, can have a single dot
            if (this.text.headCharIs('.')) {
                this.appendCurrent(token);
                if (!this.text.isDigit()) {
                    // this is no number, it is just a dot (can be dot notation)
                    token.setType("DELIMITER");
                    return true;
                }
            } else {
                this.appendAllDigits(token);
                if (this.text.isDecimalMark()) this.appendCurrent(token);
            }

            this.appendAllDigits(token);
            // check for exponential notation like "2.3e-4", "1.23e50" or "2e+4"
            if (this.text.headCharIs('E') || this.text.headCharIs('e')) {
                if (this.text.isDigit(1) || this.text.headCharIs('-', 1) || this.text.headCharIs('+', 1)) {
                    this.appendCurrent(token);

                    if (this.text.headCharIs('+') || this.text.headCharIs('-')) this.appendCurrent(token);

                    // Scientific notation MUST be followed by an exponent
                    if (!this.text.isDigit()) {
                        // throw this.createSyntaxError('Digit expected, got "' + this.currentCharacter() + '"')
                        throw new Error('Digit expected, got "' + this.text.charByHead() + '"')
                    }

                    this.appendAllDigits(token);

                    if (this.text.isDecimalMark()) {
                        // throw this.createSyntaxError('Digit expected, got "' + this.currentCharacter() + '"')
                        throw new Error('Digit expected, got "' + this.text.charByHead() + '"')
                    }
                } else if (this.text.headCharIs('.')) {
                    this.text.headForward();
                    // throw this.createSyntaxError('Digit expected, got "' + this.currentCharacter() + '"')
                    throw new Error('Digit expected, got "' + this.text.charByHead() + '"')
                }
            }

            return true;
        }

        return false;
    }

    is_variable_function_namedOperator(token: Token): TokenType {
        // check for variables, functions, named operators
        if (this.text.isAlpha()) {
            while (this.text.isAlpha() || this.text.isDigit()) this.appendCurrent(token)
            token.setType("SYMBOL");
            return true;
        }
        return false;
    }



    getErrorOnHead(token: Token): string {
        return this.text.getErrorOnIndex(token.Value.length, token.head);
    }


}