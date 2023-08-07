import { ArrayNode } from "../../node/ArrayNode"
import { ExpressionNode } from "../../node/Node";
import { parseRelational } from "./parseRelational"

/**
 * Parse a single comma-separated row from a matrix, like 'a, b, c'
 * @return {ArrayNode} node
 */
export function parseRow(state): ArrayNode {
    const params: Array<ExpressionNode> = [parseRelational(state)]

    while (state.isToken(',')) { // eslint-disable-line no-unmodified-loop-condition
        state.gotToNextToken();
        // parse expression
        params.push(parseRelational(state));
    }

    return new ArrayNode(params)
}