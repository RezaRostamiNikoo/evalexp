/**
 * Evaluated when the expression is not yet ended but expected to end
 * @return {Node} res
 * @private
 */
export function parseEnd() {
    if (this.isToken('')) {
        // syntax error or unexpected end of expression
        throw this.createSyntaxError('Unexpected end of expression')
    } else {
        throw this.createSyntaxError('Value expected')
    }
}