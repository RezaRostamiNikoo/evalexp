import { hasOwnProperty } from "../../utils/object"
import { ExpressionNode } from "../../node/ExpressionNode"
import { OperatorNode } from "../../node/OperatorNode"
import { State } from "../State"
import { parsePow } from "./parsePow"

/**
 * Unary plus and minus, and logical and bitwise not
 * @return {ExpressionNode} node
 * @private
 */
export function parseUnary(state: State): ExpressionNode {
    let name, params, fn
    const operators = {
        '-': 'unaryMinus',
        '+': 'unaryPlus',
        // '~': 'bitNot',
        // not: 'not'
    }

    if (hasOwnProperty(operators, state.token.value)) {
        fn = operators[state.token.value]
        name = state.token.value

        state.goAhead();
        params = [parseUnary(state)]

        return new OperatorNode(name, fn, params)
    }

    return parsePow(state)
}