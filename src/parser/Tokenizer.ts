import { DigitExpectedException, EmptyExpressionError, UnknownTokenException } from "../errors";
import { Token } from "./Token";
import { DELIMITERS } from "./constants";
import { TokenType } from "./types";
import { StringAnalyzer } from "./StringAnalyzer";

export class Tokenizer {
    private textAnalyzer: StringAnalyzer;
    constructor(expression: string) {
        if (!expression) throw new EmptyExpressionError()
        this.textAnalyzer = new StringAnalyzer(expression);
    }

    /**
     * Get next token in the current string expr.
     * The token and token type are available as token and tokenType
     * @private
     */
    getNextToken(): Token {

        this.textAnalyzer.skipIgnoredCharacters();
        this.textAnalyzer.skipComments(); // TODO: this skip comments maybe it is important in the future
        if (this.textAnalyzer.isEndOfText()) return undefined;

        let tokenType: TokenType =
            this.getDelimiterToken() ||
            this.get_bi_oct_hex_token() || // TODO: check if it is needed in openBrim. it should be checked exactly after delimiter and before other things
            this.getNumberToken() ||
            this.get_variable_function_namedOperator_token();

        let tokenIndex = this.textAnalyzer.head;
        let tokenString = this.textAnalyzer.tokenizeHeadToScout();

        if (!tokenType) throw new UnknownTokenException(this.textAnalyzer.generatePonterOnHead())

        return new Token(tokenString, tokenType, tokenIndex);
    }

    private getDelimiterToken(): TokenType {
        const maxLengthOfADelimiter = 4;
        for (let step = maxLengthOfADelimiter; step > 0; step--) {
            if (this.textAnalyzer.remainedCharByHead() >= step) {
                if (DELIMITERS[this.textAnalyzer.chunkFromHead(step)]) {
                    this.textAnalyzer.scoutForward(step)
                    return "DELIMITER";
                }
            }
        }
        return undefined;
    }

    get_bi_oct_hex_token(): TokenType {
        // check for binary, octal, or hex
        const c2 = this.textAnalyzer.chunkFromHead(2)
        if (c2 === '0b' || c2 === '0o' || c2 === '0x') {
            this.textAnalyzer.scoutForward(2);
            // this.appendCurrent(token, "NUMBER");
            // this.appendCurrent(token);
            while (this.textAnalyzer.isHexDigit()) this.textAnalyzer.scoutForward();

            // if (this.textAnalyzer.headCharIs('.')) {
            //     // this number has a radix point
            //     this.set(textAnalyzer, '.');

            //     // get the digits after the radix
            //     while (this.textAnalyzer.isHexDigit()) this.textAnalyzer.scoutForward();

            // } else if (this.textAnalyzer.headCharIs('i')) {
            //     // this number has a word size suffix
            //     this.set(textAnalyzer, 'i');
            //     // get the word size
            //     while (this.textAnalyzer.isDigit()) this.textAnalyzer.scoutForward()

            // }

            return "NUMBER";
        }

        return undefined;
    }

    private getNumberToken(): TokenType {
        // check for a number
        if (this.textAnalyzer.isDigitDot()) {

            // get number, can have a single dot
            if (this.textAnalyzer.scoutCharIs('.')) {
                this.textAnalyzer.scoutForward();
                if (!this.textAnalyzer.isDigit()) {
                    // this is no number, it is just a dot (can be dot notation)
                    return "DELIMITER";
                }
            } else {
                while (this.textAnalyzer.isDigit()) this.textAnalyzer.scoutForward()
                if (this.textAnalyzer.isDecimalMark()) this.textAnalyzer.scoutForward();
            }

            while (this.textAnalyzer.isDigit()) this.textAnalyzer.scoutForward()
            // check for exponential notation like "2.3e-4", "1.23e50" or "2e+4"
            if (this.textAnalyzer.scoutCharIs('E') || this.textAnalyzer.scoutCharIs('e')) {
                if (this.textAnalyzer.isDigit(1) || this.textAnalyzer.scoutCharIs('-', 1) || this.textAnalyzer.scoutCharIs('+', 1)) {
                    this.textAnalyzer.scoutForward();

                    if (this.textAnalyzer.scoutCharIs('+') || this.textAnalyzer.scoutCharIs('-')) this.textAnalyzer.scoutForward();

                    // Scientific notation MUST be followed by an exponent
                    if (!this.textAnalyzer.isDigit()) {
                        // throw this.createSyntaxError('Digit expected, got "' + this.currentCharacter() + '"')
                        throw new DigitExpectedException(this.textAnalyzer.generatePonterOnScout())
                    }

                    while (this.textAnalyzer.isDigit()) this.textAnalyzer.scoutForward()

                    if (this.textAnalyzer.isDecimalMark()) {
                        // throw this.createSyntaxError('Digit expected, got "' + this.currentCharacter() + '"')
                        throw new DigitExpectedException(this.textAnalyzer.generatePonterOnScout())
                    }
                } else if (this.textAnalyzer.scoutCharIs('.')) {
                    // throw this.createSyntaxError('Digit expected, got "' + this.currentCharacter() + '"')
                    throw new DigitExpectedException(this.textAnalyzer.generatePonterOnScout())
                }
            }

            return "NUMBER";
        }

        return undefined;
    }

    private get_variable_function_namedOperator_token(): TokenType {
        // check for variables, functions, named operators
        if (this.textAnalyzer.isAlpha()) {
            while (this.textAnalyzer.isAlpha() || this.textAnalyzer.isDigit()) this.textAnalyzer.scoutForward()
            return "SYMBOL";
        }
        return undefined;
    }



    getErrorOnHead(token: Token): string {
        return this.textAnalyzer.generatePointer(token.index, token.value.length);
    }


}