/**
 * parse the matrix
 * @return {Node} node
 * @private
 */
export function parseMatrix() {
    let array, params, rows, cols

    if (this.state.token === '[') {
        // matrix [...]
        this.openParams()
        this.getToken()

        if (!this.isToken(']')) {
            // this is a non-empty matrix
            const row = this.parseRow()

            if (this.isToken(';')) {
                // 2 dimensional array
                rows = 1
                params = [row]

                // the rows of the matrix are separated by dot-comma's
                while (this.isToken(';')) { // eslint-disable-line no-unmodified-loop-condition
                    this.getToken()

                    params[rows] = this.parseRow()
                    rows++
                }

                if (!this.isToken(']')) {
                    throw this.createSyntaxError('End of matrix ] expected')
                }
                this.closeParams()
                this.getToken()

                // check if the number of columns matches in all rows
                cols = params[0].items.length
                for (let r = 1; r < rows; r++) {
                    if (params[r].items.length !== cols) {
                        throw this.createError('Column dimensions mismatch ' +
                            '(' + params[r].items.length + ' !== ' + cols + ')')
                    }
                }

                array = new ArrayNode(params)
            } else {
                // 1 dimensional vector
                if (!this.isToken(']')) {
                    throw this.createSyntaxError('End of matrix ] expected')
                }
                this.closeParams()
                this.getToken()

                array = row
            }
        } else {
            // this is an empty matrix "[ ]"
            this.closeParams()
            this.getToken()
            array = new ArrayNode([])
        }

        return this.parseAccessors(array)
    }

    return this.parseObject()
}