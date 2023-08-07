import { createSyntaxError } from "../../errors"
import { ExpressionNode } from "../../node/ExpressionNode"

/**
 * Evaluated when the expression is not yet ended but expected to end
 * @return {ExpressionNode} res
 * @private
 */
export function parseEnd(state): ExpressionNode {
    if (!state.token) {
        // syntax error or unexpected end of expression
        throw createSyntaxError(state, 'Unexpected end of expression')
    } else {
        throw createSyntaxError(state, 'Value expected')
    }
}