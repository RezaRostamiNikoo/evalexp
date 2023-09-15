import { createSyntaxError } from "../../errors";
import { ExpressionNode } from "../../node/ExpressionNode"
import { ParenthesisNode } from "../../node/ParenthesisNode";
import { State } from "../State"
import { parseAccessors } from "./parseAccessors";
import { parseEnd } from "./parseEnd"
import { parseRelational } from "./parseRelational";

/**
 * parentheses
 * @return {ExpressionNode} node
 * @private
 */
export function parseParentheses(state: State): ExpressionNode {
    let node

    // check if it is a parenthesized expression
    if (state.isToken('(')) {
        // parentheses (...)
        state.goAhead();
        // state.nextLevel();

        node = parseRelational(state) // start again

        if (!state.isToken(')')) {
            throw createSyntaxError(state, 'Parenthesis ) expected')
        }
        // state.prevLevel();
        state.goAhead();

        node = new ParenthesisNode(node)
        node = parseAccessors(state, node)
        return node;
    }

    return parseEnd(state)
}