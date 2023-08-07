import { rule2Node } from "../../../utils/is"
import { ExpressionNode } from "../../node/Node"
import { OperatorNode } from "../../node/OperatorNode"
import { State } from "../State"
import { parsePercentage } from "./parsePercentage"

/**
 * Infamous "rule 2" as described in https://github.com/josdejong/mathjs/issues/792#issuecomment-361065370
 * And as amended in https://github.com/josdejong/mathjs/issues/2370#issuecomment-1054052164
 * Explicit division gets higher precedence than implicit multiplication
 * when the division matches this pattern:
 *   [unaryPrefixOp]?[number] / [number] [symbol]
 * @return {ExpressionNode} node
 * @private
 */
export function parseRule2(state: State): ExpressionNode {
    let node = parsePercentage(state)
    let last = node
    const tokenStates = []

    while (true) {
        // Match the "number /" part of the pattern "number / number symbol"
        if (state.isToken('/') && rule2Node(last)) {
            // Look ahead to see if the next token is a number
            tokenStates.push(Object.assign({}, state))
            state.goAHead();

            // Match the "number / number" part of the pattern
            if (state.isType("NUMBER")) {
                // Look ahead again
                tokenStates.push(Object.assign({}, state))
                state.goAHead();

                // Match the "symbol" part of the pattern, or a left parenthesis
                if (state.isType("SYMBOL") || state.isToken('(')) {
                    // We've matched the pattern "number / number symbol".
                    // Rewind once and build the "number / number" node; the symbol will be consumed later
                    state.rewind(); // one time to get to the number
                    last = parsePercentage(state)
                    node = new OperatorNode('/', 'divide', [node, last])
                } else {
                    // Not a match, so rewind
                    state.rewind().rewind(); // two times to get to the "/" delimiter again 
                    break;
                }
            } else break;
        } else {
            break
        }
    }

    return node
}