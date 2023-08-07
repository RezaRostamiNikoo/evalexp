import { hasOwnProperty } from "../../../utils/object"
import { ConstantNode } from "../../node/ConstantNode"
import { ExpressionNode } from "../../node/ExpressionNode"
import { OperatorNode } from "../../node/OperatorNode"
import { State } from "../State"
import { parseUnary } from "./parseUnary"

/**
 * percentage or mod
 * @return {ExpressionNode} node
 * @private
 */
export function parsePercentage(state: State): ExpressionNode {
    let node, name

    node = parseUnary(state)

    const operators = {
        '%': 'mod',
    }
    while (hasOwnProperty(operators, state.token.Value)) {
        name = state.token.Value
        state.goAHead();

        if (name === '%' && state.isType("DELIMITER") && !state.isToken('(')) {
            // If the expression contains only %, then treat that as /100
            node = new OperatorNode('/', 'divide', [node, new ConstantNode(100)], false, true)
        }
    }

    return node;
}