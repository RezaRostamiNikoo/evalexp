/**
 * this is from mathjs library
 * date of using: 02 Aug 2023
 * github link: https://github.com/josdejong/mathjs
 * version: npm v11.9.1 
 * commit: https://github.com/josdejong/mathjs/commit/59320053fd35e64351713c4ef32af37df1f4c425
 */

import { State } from "./State";

import { parseBlock } from "./tokenParsers/blockParser";
import { Token } from "./Token";
import { ExpressionSyntaxError } from "../errors";
import { parseNumber } from "./tokenParsers/parseNumber";

export class Parser {
    private state: State;

    /**
     * Start of the parse levels below, in order of precedence
     * @return {Node} node
     * @private
     */
    parseStart(expression: string) {
        const state = new State(expression);

        if (state.Tokens.isEmpty()) return ""; // return a StringNode

        // link tokens together
        state.Tokens.reduce((prev: Token, curr: Token, c: number, arr: []): any => {
            prev.Next = curr;
            curr.Prev = prev;
            return curr;
        });

        const token = state.Tokens.peek();

        parseBlock(state);




        ///////////////////////////////////
        if (token.Type === "NULL") throw new ExpressionSyntaxError(token.Value);
        if (token.Type === "UNKNOWN") throw new ExpressionSyntaxError(token.Value);
        if (token.Type === "NUMBER") return parseNumber(state);
        if (token.Type === "DELIMITER") return this.parse_delimiter(state);
        if (token.Type === "SYMBOL") return this.parse_SYmbol();

        parse_Number_Delimiter(token);
    }





}