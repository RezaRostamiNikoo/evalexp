import { hasOwnProperty } from "../../../utils/object"
import { ExpressionNode } from "../../node/Node"
import { OperatorNode } from "../../node/OperatorNode"
import { State } from "../State"
import { parsePow } from "./parsePow"

/**
 * Unary plus and minus, and logical and bitwise not
 * @return {Node} node
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
    const token = state.Tokens.peek();

    if (hasOwnProperty(operators, token.Value)) {
        fn = operators[token.Value]
        name = token

        this.getTokenSkipNewline()
        params = [parseUnary(state)]

        return new OperatorNode(name, fn, params)
    }

    return parsePow(state);
}