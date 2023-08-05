import { hasOwnProperty } from "../../../utils/object"
import { State } from "../State"
import { parseAddSubtract } from "./parseAddSubtract"

/**
 * Parse a chained conditional, like 'a > b >= c'
 * @return {Node} node
 */
export function parseRelational(state: State) {
    const params = [parseAddSubtract(state)]
    const conditionals = []

    const operators = {
        '==': 'equal',
        '!=': 'unequal',
        '<': 'smaller',
        '>': 'larger',
        '<=': 'smallerEq',
        '>=': 'largerEq'
    }

    while (hasOwnProperty(operators, state.tokenValue)) { // eslint-disable-line no-unmodified-loop-condition
        const cond = { name: state.tokenValue, fn: operators[state.tokenValue] }
        conditionals.push(cond)
        this.getTokenSkipNewline()
        params.push(parseAddSubtract(state))
    }

    if (params.length === 1) {
        return params[0]
    } else if (params.length === 2) {
        return new OperatorNode(conditionals[0].name, conditionals[0].fn, params)
    } else {
        return new RelationalNode(conditionals.map(c => c.fn), params)
    }
}