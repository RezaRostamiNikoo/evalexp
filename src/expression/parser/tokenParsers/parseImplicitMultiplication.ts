import { isConstantNode, isOperatorNode } from "../../../utils/is"
import { ExpressionNode } from "../../node/Node"
import { OperatorNode } from "../../node/OperatorNode"
import { TOKENTYPE } from "../constants"
import { State } from "../State"
import { parseRule2 } from "./parseRule2"

/**
 * implicit multiplication
 * @return {Node} node
 * @private
 */
export function parseImplicitMultiplication(state: State): ExpressionNode {
    let node, last

    node = parseRule2(state);
    last = node
    const token = state.Tokens.peek();

    while (true) {
        if ((token.Type === TOKENTYPE.SYMBOL) ||
            (token.Value === 'in' && isConstantNode(node)) ||
            (token.Type === TOKENTYPE.NUMBER &&
                !isConstantNode(last) &&
                (!isOperatorNode(last) || last.op === '!')) ||
            (token.Value === '(')) {
            // parse implicit multiplication
            //
            // symbol:      implicit multiplication like '2a', '(2+3)a', 'a b'
            // number:      implicit multiplication like '(2+3)2'
            // parenthesis: implicit multiplication likethis.'2(3+4)', '(3+4)(1+2)'
            last = parseRule2(state);
            node = new OperatorNode('*', 'multiply', [node, last], true /* implicit */)
        } else {
            break
        }
    }

    return node
}