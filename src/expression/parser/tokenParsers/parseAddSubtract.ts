import { hasOwnProperty } from "../../../utils/object"
import { State } from "../State"
import { parseMultiplyDivide } from "./parseMultiplyDivide"

/**
 * add or subtract
 * @return {Node} node
 * @private
 */
export function parseAddSubtract(state: State) {
    let node, name, fn, params

    node = parseMultiplyDivide(state)

    const operators = {
        '+': 'add',
        '-': 'subtract'
    }
    while (hasOwnProperty(operators, this.state.token)) {
        name = this.state.token
        fn = operators[name]

        this.getTokenSkipNewline()
        const rightNode = this.parseMultiplyDivide()
        if (rightNode.isPercentage) {
            params = [node, new OperatorNode('*', 'multiply', [node, rightNode])]
        } else {
            params = [node, rightNode]
        }
        node = new OperatorNode(name, fn, params)
    }

    return node
}