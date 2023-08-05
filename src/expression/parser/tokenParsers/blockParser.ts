import { ConstantNode } from "../../node/ConstantNode";
import { State } from "../State";
import { parseRelational } from "./parseRelational";

/**
  * Parse a block with expressions. Expressions can be separated by a newline
  * character '\n', or by a semicolon ';'. In case of a semicolon, no output
  * of the preceding line is returned.
  * @return {Node} node
  * @private
  */
export function parseBlock(state: State) {
    let node;

    const token = state.tokenValue;

    if (token !== '') {
        node = parseRelational(state)
        if (this.state.comment) {
            node.comment = this.state.comment
        }
    }

    if (!node) {
        node = new ConstantNode(undefined)
        if (this.state.comment) {
            node.comment = this.state.comment
        }
    }

    return node
}