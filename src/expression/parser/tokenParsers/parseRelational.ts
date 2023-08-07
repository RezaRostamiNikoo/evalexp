import { hasOwnProperty } from "../../../utils/object"
import { ExpressionNode } from "../../node/ExpressionNode"
import { OperatorNode } from "../../node/OperatorNode"
import { RelationalNode } from "../../node/RelationalNode"
import { State } from "../State"
import { parseAddSubtract } from "./parseAddSubtract"

/**
 * Parse a chained conditional, like 'a > b >= c'
 * @return {Node} node
 */
export function parseRelational(state: State): ExpressionNode {

    const params = [parseAddSubtract(state)]
    const conditionals = []

    const token = state.token;
    const operators = {
        '.EQ.': 'equal',
        '.NE.': 'unequal',
        '.LT.': 'smaller',
        '.GT.': 'larger',
        '.LE.': 'smallerEq',
        '.GE.': 'largerEq',

        '==': 'equal',
        '!=': 'unequal',
        '<': 'smaller',
        '>': 'larger',
        '<=': 'smallerEq',
        '>=': 'largerEq',
    }

    while (hasOwnProperty(operators, token.Value)) { // eslint-disable-line no-unmodified-loop-condition
        const cond = { name: token, fn: operators[token.Value] }
        conditionals.push(cond)
        params.push(parseAddSubtract(state))
    }

    if (params.length === 1) {
        return params[0]
    } else if (params.length === 2) {
        state.goAHead();
        return new OperatorNode(conditionals[0].name, conditionals[0].fn, params)
    } else {
        state.goAHead();
        return new RelationalNode(conditionals.map(c => c.fn), params)
    }
}