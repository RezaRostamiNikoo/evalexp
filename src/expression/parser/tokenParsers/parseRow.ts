import { parseRelational } from "./parseRelational"

/**
 * Parse a single comma-separated row from a matrix, like 'a, b, c'
 * @return {ArrayNode} node
 */
export function parseRow() {
    const params = [parseRelational(state)]
    let len = 1

    while (this.state.token === ',') { // eslint-disable-line no-unmodified-loop-condition
        this.getToken()

        // parse expression
        params[len] = parseRelational(state)
        len++
    }

    return new ArrayNode(params)
}