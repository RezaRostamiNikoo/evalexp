/**
 * parse a number
 * @return {Node} node
 * @private
 */
export function parseNumber() {
    let numberStr

    if (this.state.tokenType === TOKENTYPE.NUMBER) {
        // this is a number
        numberStr = this.state.token
        this.getToken()

        return new ConstantNode(numeric(numberStr, config.number))
    }

    return this.parseParentheses()
}
