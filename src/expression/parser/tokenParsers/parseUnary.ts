import { hasOwnProperty } from "../../../utils/object"
import { State } from "../State"
import { parsePow } from "./parsePow"

/**
 * Unary plus and minus, and logical and bitwise not
 * @return {Node} node
 * @private
 */
export function parseUnary(state: State) {
    let name, params, fn
    const operators = {
        '-': 'unaryMinus',
        '+': 'unaryPlus',
        // '~': 'bitNot',
        // not: 'not'
    }

    if (hasOwnProperty(operators, this.state.token)) {
        fn = operators[this.state.token]
        name = this.state.token

        this.getTokenSkipNewline()
        params = [parseUnary(state)]

        return new OperatorNode(name, fn, params)
    }

    return parsePow(state);
}