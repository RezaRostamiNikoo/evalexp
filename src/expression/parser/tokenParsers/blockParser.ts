import { ConstantNode } from "../../node/ConstantNode";
import { ExpressionNode } from "../../node/Node"
import { parseRelational } from "./parseRelational"

/**
 * Parse a block with expressions. Expressions can be separated by a newline
 * character '\n', or by a semicolon ';'. In case of a semicolon, no output
 * of the preceding line is returned.
 * @return {ExpressionNode} node
 * @private
 */
export function parseBlock(state): ExpressionNode {
  let result: ExpressionNode;
  if (state.token) result = parseRelational(state);
  if (!result) result = new ConstantNode(undefined);
  return result;
}