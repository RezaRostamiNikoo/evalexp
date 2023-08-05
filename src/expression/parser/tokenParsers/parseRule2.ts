import { rule2Node } from "../../../utils/is"
import { TOKENTYPE } from "../../constants"
import { State } from "../State"
import { parsePercentage } from "./parsePercentage"

/**
 * Infamous "rule 2" as described in https://github.com/josdejong/mathjs/issues/792#issuecomment-361065370
 * And as amended in https://github.com/josdejong/mathjs/issues/2370#issuecomment-1054052164
 * Explicit division gets higher precedence than implicit multiplication
 * when the division matches this pattern:
 *   [unaryPrefixOp]?[number] / [number] [symbol]
 * @return {Node} node
 * @private
 */
export function parseRule2(state: State) {
    let node = parsePercentage(state)
    let last = node
    const tokenStates = []

    while (true) {
        // Match the "number /" part of the pattern "number / number symbol"
        if (state.token.equal('/') && rule2Node(last)) {
            // Look ahead to see if the next token is a number
            tokenStates.push(Object.assign({}, state))
            state.token.getTokenSkipNewline()

            // Match the "number / number" part of the pattern
            if (state.tokenType === TOKENTYPE.NUMBER) {
                // Look ahead again
                tokenStates.push(Object.assign({}, state))
                state.token.getTokenSkipNewline()

                // Match the "symbol" part of the pattern, or a left parenthesis
                if (state.tokenType === TOKENTYPE.SYMBOL || state.token.equal('(')) {
                    // We've matched the pattern "number / number symbol".
                    // Rewind once and build the "number / number" node; the symbol will be consumed later
                    Object.assign(state, tokenStates.pop())
                    tokenStates.pop()
                    last = parsePercentage(state)
                    node = new OperatorNode('/', 'divide', [node, last])
                } else {
                    // Not a match, so rewind
                    tokenStates.pop()
                    Object.assign(state, tokenStates.pop())
                    break
                }
            } else {
                // Not a match, so rewind
                Object.assign(state, tokenStates.pop())
                break
            }
        } else {
            break
        }
    }

    return node;
}