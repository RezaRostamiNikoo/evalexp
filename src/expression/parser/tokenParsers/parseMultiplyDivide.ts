import { hasOwnProperty } from "../../../utils/object"
import { State } from "../State"
import { parseImplicitMultiplication } from "./parseImplicitMultiplication"

/**
 * multiply, divide, modulus
 * @return {Node} node
 * @private
 */
export function parseMultiplyDivide(state: State) {
    let node, last, name, fn

    node = parseImplicitMultiplication(state )
    last = node

    const operators = {
        '*': 'multiply',
        '.*': 'dotMultiply',
        '/': 'divide',
        './': 'dotDivide'
    }

    while (true) {
        if (hasOwnProperty(operators, this.state.token)) {
            // explicit operators
            name = this.state.token
            fn = operators[name]

            this.getTokenSkipNewline()

            last = this.parseImplicitMultiplication()
            node = new OperatorNode(name, fn, [node, last])
        } else {
            break
        }
    }

    return node
}