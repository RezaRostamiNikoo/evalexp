import { State } from "../State"
import { parseLeftHandOperators } from "./parseLeftHandOperators"

/**
 * power
 * Note: power operator is right associative
 * @return {Node} node
 * @private
 */
export function parsePow(state: State) {
    let node, name, fn, params

    node = parseLeftHandOperators(state)

    if (this.state.token === '^') {
        name = this.state.token
        fn = 'pow'
        this.getTokenSkipNewline()
        params = [node, this.parseUnary()] // Go back to unary, we can have '2^-3'
        node = new OperatorNode(name, fn, params)
    }

    return node;
}