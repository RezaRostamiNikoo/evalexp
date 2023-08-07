import { isOperatorNode } from "../../../utils/is"
import { hasOwnProperty } from "../../../utils/object"
import { ExpressionNode } from "../../node/Node"
import { OperatorNode } from "../../node/OperatorNode"
import { State } from "../State"
import { parseImplicitMultiplication } from "./parseImplicitMultiplication"

/**
 * multiply, divide, modulus
 * @return {Node} node
 * @private
 */
export function parseMultiplyDivide(state: State): ExpressionNode {
    let node, last, name, fn

    node = parseImplicitMultiplication(state)
    last = node;
    const token = state.Tokens.peek().Value;

    const operators = {
        '*': 'multiply',
        // '.*': 'dotMultiply',
        '/': 'divide',
        // './': 'dotDivide'
    }

    while (true) {
        if (hasOwnProperty(operators, token)) {
            last = parseImplicitMultiplication(state);
            node = new OperatorNode(token, operators[token], [node, last])
        } else {
            break
        }
    }

    return node
}