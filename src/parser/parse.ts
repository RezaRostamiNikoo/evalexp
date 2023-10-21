/**
 * this is from mathjs library
 * date of using: 02 Aug 2023
 * github link: https://github.com/josdejong/mathjs
 * version: npm v11.9.1 
 * commit: https://github.com/josdejong/mathjs/commit/59320053fd35e64351713c4ef32af37df1f4c425
 */

import { State } from "./State";

import { parseBlock } from "./tokenParsers/blockParser";
import { ExpressionNode } from "../node/ExpressionNode";

/**
 * Start of the parse levels below, in order of precedence
 * @return {ExpressionNode} node
 * @private
 */
export function parse(expression: string): ExpressionNode {
    const state = new State(expression);

    if (!state.token.value) throw new Error("there is no token");

    return parseBlock(state);
}