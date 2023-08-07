import { ConstantNode } from "../../node/ConstantNode";
import { ExpressionNode } from "../../node/Node";
import { State } from "../State";
import { parseRelational } from "./parseRelational";

/**
  * Parse a block with expressions. Expressions can be separated by a newline
  * character '\n', or by a semicolon ';'. In case of a semicolon, no output
  * of the preceding line is returned.
  * @return {Node} node
  * @private
  */
export function parseBlock(state: State): ExpressionNode {
    let result: ExpressionNode;
    const token = state.Tokens.peek().Value;
    if (token !== undefined) result = parseRelational(state)
    if (!result) result = new ConstantNode(undefined) // TODO: replace it with a throw an Error
    return result
}