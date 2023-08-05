/**
 * this is from mathjs library
 * date of using: 02 Aug 2023
 * github link: https://github.com/josdejong/mathjs
 * version: npm v11.9.1 
 * commit: https://github.com/josdejong/mathjs/commit/59320053fd35e64351713c4ef32af37df1f4c425
 */

import { isAccessorNode, isConstantNode, isFunctionNode, isOperatorNode, isSymbolNode } from "../../utils/is";
import { hasOwnProperty } from "../../utils/object";
import { CONSTANTS, DELIMITERS, NAMED_DELIMITERS, NUMERIC_CONSTANTS, TOKENTYPE } from "../constants"
import { State } from "./State";

import * as helper from "./helper";
import { parseBlock } from "./tokenParsers/blockParser";

export class Parser {
    private state: State;

    /**
     * Start of the parse levels below, in order of precedence
     * @return {Node} node
     * @private
     */
    parseStart(expression: string, extraNodes) {
        const state = new State(expression, extraNodes);
        state.getToken()

        const node = parseBlock(this.state);

        // check for garbage at the end of the expression
        // an expression ends with a empty character '' and tokenType DELIMITER
        if (!this.state.token.equal('')) {
            if (this.state.tokenType === TOKENTYPE.DELIMITER) {
                // user entered a not existing operator like "//"

                // TODO: give hints for aliases, for example with "<>" give as hint " did you mean !== ?"
                throw this.createError('Unexpected operator ' + this.state.tokenValue)
            } else {
                throw this.createSyntaxError('Unexpected part "' + this.state.tokenValue + '"')
            }
        }

        return node
    }

    /**
     * Shortcut for getting the current col value (one based)
     * Returns the column (position) where the last this.state.token starts
     * @private
     */
    col() {
        return this.state.index - this.state.tokenValue.length + 1
    }



    // // Now that we can parse, automatically convert strings to Nodes by parsing
    // typed.addConversion({ from: 'string', to: 'Node', convert: parse })

}