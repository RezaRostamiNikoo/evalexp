import { createSyntaxError } from "../../errors";
import { ArrayNode } from "../../node/ArrayNode";
import { ExpressionNode } from "../../node/ExpressionNode"
import { State } from "../State"
import { parseAccessors } from "./parseAccessors";
import { parseNumber } from "./parseNumber";
import { parseRow } from "./parseRow";

/**
 * parse the matrix
 * @return {ExpressionNode} node
 * @private
 */
export function parseArray(state: State): ExpressionNode {
    let array, params, rows, cols

    if (state.isToken('[')) {
        // matrix [...]
        state.goAhead();
        // state.nextLevel();

        if (!state.isToken(']')) {
            // this is a non-empty matrix
            const row = parseRow(state)
            // 1 dimensional vector
            if (!state.isToken(']')) {
                throw createSyntaxError(state, 'End of matrix ] expected')
            }
            // state.prevLevel();
            state.goAhead();

            array = row;
        } else {
            // this is an empty matrix "[ ]"
            // state.prevLevel();
            state.goAhead();
            array = new ArrayNode([])
        }

        return parseAccessors(state, array)
    }

    return parseNumber(state);
}