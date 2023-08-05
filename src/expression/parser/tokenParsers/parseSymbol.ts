/**
 * parse symbols: functions, variables, constants, units
 * @return {Node} node
 * @private
 */
export function parseSymbol() {
    let node, name

    if (this.state.tokenType === TOKENTYPE.SYMBOL ||
        (this.state.tokenType === TOKENTYPE.DELIMITER && this.state.token in NAMED_DELIMITERS)) {
        name = this.state.token

        this.getToken()

        if (hasOwnProperty(CONSTANTS, name)) { // true, false, null, ...
            console.log("node = new ConstantNode(CONSTANTS[name])", CONSTANTS, name);
            // node = new ConstantNode(CONSTANTS[name])
        } else if (NUMERIC_CONSTANTS.indexOf(name) !== -1) { // NaN, Infinity
            console.log("node = new ConstantNode(numeric(name, 'number'))", NUMERIC_CONSTANTS, name);
            // node = new ConstantNode(numeric(name, 'number'))
        } else {
            console.log("node = new SymbolNode(name)", name);
            // node = new SymbolNode(name)
        }

        // parse function parameters and matrix index
        node = this.parseAccessors(node)
        return node
    }

    return this.parseDoubleQuotesString()
}