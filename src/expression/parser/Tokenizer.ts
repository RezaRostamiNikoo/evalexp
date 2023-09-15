import { DigitExpectedException, EmptyExpressionError, UnknownTokenException } from "../errors";
import { Token } from "./Token";
import { DELIMITERS } from "./constants";
import { TokenType } from "./types";
import { StringAnalyzer } from "./StringAnalyzer";

export class Tokenizer {
    private text: StringAnalyzer;
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

        this.text.skipIgnoredCharacters();
        this.text.skipComments(); // TODO: this skip comments maybe it is important in the future
        if (this.text.isEndOfText()) return undefined;

        let tokenType: TokenType =
            this.getDelimiterToken() ||
            // if (this.is_bi_oct_hex(result)) return result; // TODO: check if it is needed in openBrim. it should be checked exactly after delimiter and before other things
            this.getNumberToken() ||
            this.get_variable_function_namedOperator_token();

        let tokenIndex = this.text.head;
        let tokenString = this.text.tokenizeHeadToScout();

        if (!tokenType) throw new UnknownTokenException(this.text.generatePonterOnHead())

        return new Token(tokenString, tokenType, tokenIndex);
    }

    getDelimiterToken(): TokenType {
        const maxLengthOfADelimiter = 4;
        for (let step = maxLengthOfADelimiter; step > 0; step--) {
            if (this.text.headToEndLength() >= step) {
                if (DELIMITERS[this.text.chunkFromHead(step)]) {
                    this.text.scoutForward(step)
                    return "DELIMITER";
                }
            }
        }
        return undefined;
    }

    // is_bi_oct_hex(token: Token): TokenType {
    //     // check for binary, octal, or hex
    //     const c2 = this.text.chunkFromHead(2)
    //     if (c2 === '0b' || c2 === '0o' || c2 === '0x') {
    //         this.text.scoutForward(2);
    //         // this.appendCurrent(token, "NUMBER");
    //         // this.appendCurrent(token);
    //         while (this.text.isHexDigit()) this.text.scoutForward();

    //         if (this.text.headCharIs('.')) {
    //             // this number has a radix point
    //             this.set(token, '.');

    //             // get the digits after the radix
    //             while (this.text.isHexDigit()) this.text.scoutForward();

    //         } else if (this.text.headCharIs('i')) {
    //             // this number has a word size suffix
    //             this.set(token, 'i');
    //             // get the word size
    //             while (this.text.isDigit()) this.text.scoutForward()

    //         }

    //         return true;
    //     }

    //     return false;
    // }

    getNumberToken(): TokenType {
        // check for a number
        if (this.text.isDigitDot()) {

            // get number, can have a single dot
            if (this.text.scoutCharIs('.')) {
                this.text.scoutForward();
                if (!this.text.isDigit()) {
                    // this is no number, it is just a dot (can be dot notation)
                    return "DELIMITER";
                }
            } else {
                while (this.text.isDigit()) this.text.scoutForward()
                if (this.text.isDecimalMark()) this.text.scoutForward();
            }

            while (this.text.isDigit()) this.text.scoutForward()
            // check for exponential notation like "2.3e-4", "1.23e50" or "2e+4"
            if (this.text.scoutCharIs('E') || this.text.scoutCharIs('e')) {
                if (this.text.isDigit(1) || this.text.scoutCharIs('-', 1) || this.text.scoutCharIs('+', 1)) {
                    this.text.scoutForward();

                    if (this.text.scoutCharIs('+') || this.text.scoutCharIs('-')) this.text.scoutForward();

                    // Scientific notation MUST be followed by an exponent
                    if (!this.text.isDigit()) {
                        // throw this.createSyntaxError('Digit expected, got "' + this.currentCharacter() + '"')
                        throw new DigitExpectedException(this.text.generatePonterOnScout())
                    }

                    while (this.text.isDigit()) this.text.scoutForward()

                    if (this.text.isDecimalMark()) {
                        // throw this.createSyntaxError('Digit expected, got "' + this.currentCharacter() + '"')
                        throw new DigitExpectedException(this.text.generatePonterOnScout())
                    }
                } else if (this.text.scoutCharIs('.')) {
                    // throw this.createSyntaxError('Digit expected, got "' + this.currentCharacter() + '"')
                    throw new DigitExpectedException(this.text.generatePonterOnScout())
                }
            }

            return "NUMBER";
        }

        return undefined;
    }

    get_variable_function_namedOperator_token(): TokenType {
        // check for variables, functions, named operators
        if (this.text.isAlpha()) {
            while (this.text.isAlpha() || this.text.isDigit()) this.text.scoutForward()
            return "SYMBOL";
        }
        return undefined;
    }



    getErrorOnHead(token: Token): string {
        return this.text.generatePointer(token.index, token.Value.length);
    }


}