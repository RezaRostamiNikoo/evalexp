import { hasOwnProperty } from "../../utils/object";
import { EmptyExpressionError, ExpressionSyntaxError } from "../errors";
import { Token } from "./Token";
import { DELIMITERS } from "./constants";
import { isAlpha, isDecimalMark, isDigit, isDigitDot, isHexDigit, isWhitespace } from "../../utils/helper";
import { TokenType } from "./types";
import { StringChar } from "../../utils/StringChar";

export class Expression {
    private text: StringChar;

    constructor(expression: string) {
        if (!expression) throw new EmptyExpressionError()
        this.text = new StringChar(expression);
    }

    /**
     * Get next token in the current string expr.
     * The token and token type are available as token and tokenType
     * @private
     */
    getNextToken(): Token {
        const result = new Token('', "NULL");
        this.text.skipIgnoredCharacters();
        if (this.isEnd(result)) return result;
        if (this.isDelimiter(result)) return result;
        if (this.is_bi_oct_hex(result)) return result;
        if (this.isNumber(result)) return result;
        if (this.is_variable_function_namedOperator(result)) return result;

        // something unknown is found, wrong characters -> a syntax error
        result.setType("UNKNOWN");
        while (!this.text.currentIs('')) {
            this.appendCurrent(result);
        }
        return undefined;
        throw new ExpressionSyntaxError(result.Value)
    }

    /**
     * it checks if there is any more character to checks. otherwise this means that it is at the end
     * @returns {boolean} true if it is the end 
     */
    isEnd(token: Token): boolean {
        // check for end of expression
        if (this.text.isEnd()) {
            // token is still empty
            token.setType("DELIMITER");
            return true;
        }
        return false;
    }

    isDelimiter(token: Token) {
        const maxLengthOfADelimiter = 4;
        for (let step = maxLengthOfADelimiter; step > 0; step--) {
            if (this.text.isDelimiter(step)) {
                this.set(token, this.text.getString(step), "DELIMITER");
                return true;
            }
        }
        return false;
    }

    is_bi_oct_hex(token: Token): boolean {
        // check for binary, octal, or hex
        const c2 = this.text.getString(2)
        if (c2 === '0b' || c2 === '0o' || c2 === '0x') {
            this.appendCurrent(token, "NUMBER");
            this.appendCurrent(token);
            this.appendAllHexDigits(token);

            if (this.text.currentIs('.')) {
                // this number has a radix point
                this.set(token, '.');

                // get the digits after the radix
                this.appendAllHexDigits(token)

            } else if (this.text.currentIs('i')) {
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

    isNumber(token: Token) {
        // check for a number
        if (this.text.isDigitDot()) {
            token.setType("NUMBER");

            // get number, can have a single dot
            if (this.text.currentIs('.')) {
                this.appendCurrent(token);

                if (!this.text.isDigit()) {
                    // this is no number, it is just a dot (can be dot notation)
                    token.setType("DELIMITER");
                    return false;
                }
            } else {
                this.appendAllDigits(token);
                if (this.text.isDecimalMark()) this.appendCurrent(token);
            }

            this.appendAllDigits(token);
            // check for exponential notation like "2.3e-4", "1.23e50" or "2e+4"
            if (this.text.currentIs('E') || this.text.currentIs('e')) {
                if (this.text.isDigit(1) || this.text.nextIs('-') || this.text.nextIs('+')) {
                    this.appendCurrent(token);

                    if (this.text.currentIs('+') || this.text.currentIs('-')) this.appendCurrent(token);

                    // Scientific notation MUST be followed by an exponent
                    if (!this.text.isDigit()) {
                        // throw this.createSyntaxError('Digit expected, got "' + this.currentCharacter() + '"')
                        throw new Error('Digit expected, got "' + this.text.current() + '"')
                    }

                    this.appendAllDigits(token);

                    if (this.text.isDecimalMark()) {
                        // throw this.createSyntaxError('Digit expected, got "' + this.currentCharacter() + '"')
                        throw new Error('Digit expected, got "' + this.text.current() + '"')
                    }
                } else if (this.text.nextIs('.')) {
                    this.text.incrementIndex()
                    // throw this.createSyntaxError('Digit expected, got "' + this.currentCharacter() + '"')
                    throw new Error('Digit expected, got "' + this.text.current() + '"')
                }
            }

            return true;
        }

        return false;
    }

    is_variable_function_namedOperator(token: Token): boolean {
        // check for variables, functions, named operators
        if (this.text.isAlpha()) {
            while (this.text.isAlpha() || this.text.isDigit()) this.appendCurrent(token)
            token.setType("SYMBOL");
            return true;
        }
        return false;
    }




}