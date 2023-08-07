import { hasOwnProperty } from "../../../utils/object"
import { OperatorNode } from "../../node/OperatorNode"
import { State } from "../State"
import { parseMultiplyDivide } from "./parseMultiplyDivide"

/**
 * add or subtract
 * @return {Node} node
 * @private
 */
export function parseAddSubtract(state: State) {
    let node, name, fn, params

    node = parseMultiplyDivide(state)

    const token = state.Tokens.peek().Value;

    const operators = {
        '+': 'add',
        '-': 'subtract'
    }

    while (hasOwnProperty(operators, token)) {
        fn = operators[token];

        const rightNode = this.parseMultiplyDivide()
        if (rightNode.isPercentage) {
            params = [node, new OperatorNode('*', 'multiply', [node, rightNode])]
        } else {
            params = [node, rightNode]
        }
        node = new OperatorNode(token, fn, params)
    }

    return node
}