import { hasOwnProperty } from "../../../utils/object";
import { ExpressionNode } from "../../node/Node"
import { OperatorNode } from "../../node/OperatorNode";
import { State } from "../State"
import { parseMultiplyDivide } from "./parseMultiplyDivide"

/**
 * add or subtract
 * @return {ExpressionNode} node
 * @private
 */
export function parseAddSubtract(state: State): ExpressionNode {
    let node, name, fn, params

    node = parseMultiplyDivide(state);

    const operators = {
        '+': 'add',
        '-': 'subtract'
    }
    while (hasOwnProperty(operators, state.token.Value)) {
        name = state.token
        fn = operators[name]

        state.goAHead();

        const rightNode = parseMultiplyDivide(state)
        if ((rightNode as OperatorNode).isPercentage) {
            params = [node, new OperatorNode('*', 'multiply', [node, rightNode])]
        } else {
            params = [node, rightNode]
        }
        node = new OperatorNode(name, fn, params)
    }

    return node;
}