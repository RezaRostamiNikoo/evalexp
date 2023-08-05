/**
 * parse accessors:
 * - function invocation in round brackets (...), for example this.sqrt(2)
 * - index enclosed in square brackets [...], for example A[2,3]
 * - dot notation for properties, like foo.bar
 * @param {Object} state
 * @param {Node} node    Node on which to apply the parameters. If there
 *                       are no parameters in the expression, the node
 *                       itself is returned
 * @param {string[]} [types]  Filter the types of notations
 *                            can be ['(', '[', '.']
 * @return {Node} node
 * @private
 */
export function parseAccessors(node, types?) {
    let params

    while ((this.state.token === '(' || this.state.token === '[' || this.state.token === '.') &&
        (!types || types.indexOf(this.state.token) !== -1)) { // eslint-disable-line no-unmodified-loop-condition
        params = []

        if (this.isToken('(')) {
            if (isSymbolNode(node) || isAccessorNode(node)) {
                // function invocation likethis.fn(2, 3) or obj.fn(2, 3)
                this.openParams()
                this.getToken()

                if (!this.isToken(')')) {
                    params.push(this.parseAssignment())

                    // parse a list with parameters
                    while (this.isToken(',')) { // eslint-disable-line no-unmodified-loop-condition
                        this.getToken()
                        params.push(this.parseAssignment())
                    }
                }

                if (!this.isToken(')')) {
                    throw this.createSyntaxError('Parenthesis ) expected')
                }
                this.closeParams()
                this.getToken()

                node = new FunctionNode(node, params)
            } else {
                // implicit multiplication like (2+3)(4+5) orthis.sqrt(2)(1+2)
                // don't parse it here but let it be handled by parseImplicitMultiplication
                // with correct precedence
                return node
            }
        } else if (this.state.token === '[') {
            // index notation like variable[2, 3]
            this.openParams()
            this.getToken()

            if (!this.isToken(']')) {
                params.push(this.parseAssignment())

                // parse a list with parameters
                while (this.isToken(',')) { // eslint-disable-line no-unmodified-loop-condition
                    this.getToken()
                    params.push(this.parseAssignment())
                }
            }

            if (!this.isToken(']')) {
                throw this.createSyntaxError('Parenthesis ] expected')
            }
            this.closeParams()
            this.getToken()

            node = new AccessorNode(node, new IndexNode(params))
        } else {
            // dot notation like variable.prop
            this.getToken()

            if (this.state.tokenType !== TOKENTYPE.SYMBOL) {
                throw this.createSyntaxError('Property name expected after dot')
            }
            params.push(new ConstantNode(this.state.token))
            this.getToken()

            const dotNotation = true
            node = new AccessorNode(node, new IndexNode(params, dotNotation))
        }
    }

    return node
}