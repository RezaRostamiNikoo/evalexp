import { hasOwnProperty } from "../../../utils/object"
import { ConstantNode } from "../../node/ConstantNode"
import { OperatorNode } from "../../node/OperatorNode"
import { TOKENTYPE } from "../constants"
import { State } from "../State"
import { parseUnary } from "./parseUnary"

/**
 * percentage or mod
 * @return {Node} node
 * @private
 */
export function parsePercentage(state: State) {
    let node, name, fn, params

    node = parseUnary(state)
    const token = state.Tokens.peek();

    const operators = {
        '%': 'mod',
        mod: 'mod'
    }
    while (hasOwnProperty(operators, token.Value)) {
        fn = operators[token.Value]

        if (name === '%' && token.Type === TOKENTYPE.DELIMITER && token !== '(') {
            // If the expression contains only %, then treat that as /100
            node = new OperatorNode('/', 'divide', [node, new ConstantNode(100)], false, true)
        } else {
            params = [node, parseUnary(state)]
            node = new OperatorNode(name, fn, params)
        }
    }

    return node
}