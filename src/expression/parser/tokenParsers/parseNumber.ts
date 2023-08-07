import { ConstantNode } from "../../node/ConstantNode";
import { ExpressionNode } from "../../node/ExpressionNode"
import { State } from "../State"
import { parseParentheses } from "./parseParentheses";

/**
 * parse a number
 * @return {ExpressionNode} node
 * @private
 */
export function parseNumber(state: State): ExpressionNode {
    let numberStr

    if (state.isType("NUMBER")) {
        // this is a number
        numberStr = state.token.Value
        state.goAHead();

        // TODO: correct this numeric(...,...)
        // return new ConstantNode(numeric(numberStr, config.number))
        return new ConstantNode(numberStr);
    }

    return parseParentheses(state)
}