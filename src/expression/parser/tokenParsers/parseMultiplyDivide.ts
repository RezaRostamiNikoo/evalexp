import { hasOwnProperty } from "../../../utils/object"
import { ExpressionNode } from "../../node/ExpressionNode"
import { OperatorNode } from "../../node/OperatorNode"
import { State } from "../State"
import { parseImplicitMultiplication } from "./parseImplicitMultiplication"

/**
 * multiply, divide, modulus
 * @return {ExpressionNode} node
 * @private
 */
export function parseMultiplyDivide(state: State): ExpressionNode {
    let node, last, name, fn

    node = parseImplicitMultiplication(state)
    last = node

    const operators = {
        '*': 'multiply',
        // '.*': 'dotMultiply',
        '/': 'divide',
        // './': 'dotDivide'
    }

    while (true) {
        if (hasOwnProperty(operators, state.token.Value)) {
            // explicit operators
            name = state.token.Value
            fn = operators[name]

            state.goAHead();

            last = parseImplicitMultiplication(state)
            node = new OperatorNode(name, fn, [node, last])
        } else {
            break
        }
    }

    return node;
}