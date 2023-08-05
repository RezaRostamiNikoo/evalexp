/**
 * Parse a double quotes string.
 * @return {Node} node
 * @private
 */
export function parseDoubleQuotesString() {
    let node, str

    if (this.state.token === '"') {
        str = this.parseDoubleQuotesStringToken()

        // create constant
        node = new ConstantNode(str)

        // parse index parameters
        node = this.parseAccessors(node)

        return node
    }

    return this.parseSingleQuotesString()
}