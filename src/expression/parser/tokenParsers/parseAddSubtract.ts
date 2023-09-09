import { isReliableNodeForUnary } from "../../../utils/is";
import { hasOwnProperty } from "../../../utils/object";
import { ExpressionNode } from "../../node/ExpressionNode"
import { OperatorNode } from "../../node/OperatorNode";
import { State } from "../State"
import { parseMultiplyDivide } from "./parseMultiplyDivide"

/**
 * add or subtract
 * @return {ExpressionNode} node
 * @private
 */
export function parseAddSubtract(state: State): ExpressionNode {
    let node, fn, params

    node = parseMultiplyDivide(state);

    const operators = {
        '+': 'add',
        '-': 'subtract'
    }

    while (hasOwnProperty(operators, state.token.Value)) {
        const token = state.token;
        fn = operators[token.Value]


        state.goAHead();

        const rightNode = parseMultiplyDivide(state)

        if ((rightNode as OperatorNode).isPercentage) {
            params = [node, new OperatorNode('*', 'multiply', [node, rightNode])]
        } else {
            params = [node, rightNode]
        }
        node = new OperatorNode(token.Value, fn, params)
    }

    return node;
}