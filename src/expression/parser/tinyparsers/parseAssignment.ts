import { State } from "../State"

/**
 * Assignment of a function or variable,
 * - can be a variable like 'a=2.3'
 * - or a updating an existing variable likethis.'matrix(2,3:5)=[6,7,8]'
 * - defining a function likethis.'f(x) = x^2'
 * @return {Node} node
 * @private
 */
parseAssignment(state: State) {
    let name, args, value, valid

    const node = this.parseConditional()

    if (this.isToken('=')) {
        if (isSymbolNode(node)) {
            // parse a variable assignment like 'a = 2/3'
            name = node.name
            this.getTokenSkipNewline()
            value = this.parseAssignment()
            return new AssignmentNode(new SymbolNode(name), value)
        } else if (isAccessorNode(node)) {
            // parse a matrix subset assignment like 'A[1,2] = 4'
            this.getTokenSkipNewline()
            value = this.parseAssignment()
            return new AssignmentNode(node.object, node.index, value)
        } else if (isFunctionNode(node) && isSymbolNode(node.fn)) {
            // parse function assignment likethis.'f(x) = x^2'
            valid = true
            args = []

            name = node.name
            node.args.forEach(function (arg, index) {
                if (isSymbolNode(arg)) {
                    args[index] = arg.name
                } else {
                    valid = false
                }
            })

            if (valid) {
                this.getTokenSkipNewline()
                value = this.parseAssignment()
                return new FunctionAssignmentNode(name, args, value)
            }
        }

        throw this.createSyntaxError('Invalid left hand side of assignment operator =')
    }

    return node
}