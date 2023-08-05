import { hasOwnProperty } from "../../../utils/object"
import { State } from "../State"
import { parseAccessors } from "./parseAccessors"
import { parseCustomNodes } from "./parseCustomNodes"

/**
 * Left hand operators: factorial x!, ctranspose x'
 * @return {Node} node
 * @private
 */
export function parseLeftHandOperators(state: State) {
    let node, name, fn, params

    node = parseCustomNodes(state)

    const operators = {
        '!': 'factorial',
        '\'': 'ctranspose'
    }

    while (hasOwnProperty(operators, this.state.token)) {
        name = this.state.token
        fn = operators[name]

        this.getToken()
        params = [node]

        node = new OperatorNode(name, fn, params)
        node = parseAccessors(node)
    }

    return node
}