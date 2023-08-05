import { hasOwnProperty } from "../../../utils/object"
import { TOKENTYPE } from "../../constants"
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

    const operators = {
        '%': 'mod',
        mod: 'mod'
    }
    while (hasOwnProperty(operators, this.state.token)) {
        name = this.state.token
        fn = operators[name]

        this.getTokenSkipNewline()

        if (name === '%' && this.state.tokenType === TOKENTYPE.DELIMITER && this.state.token !== '(') {
            // If the expression contains only %, then treat that as /100
            node = new OperatorNode('/', 'divide', [node, new ConstantNode(100)], false, true)
        } else {
            params = [node, this.parseUnary()]
            node = new OperatorNode(name, fn, params)
        }
    }

    return node
}