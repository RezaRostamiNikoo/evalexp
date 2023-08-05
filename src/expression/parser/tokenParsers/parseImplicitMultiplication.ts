import { isConstantNode, isOperatorNode } from "../../../utils/is"
import { TOKENTYPE } from "../../constants"
import { State } from "../State"
import { parseRule2 } from "./parseRule2"

/**
 * implicit multiplication
 * @return {Node} node
 * @private
 */
export function parseImplicitMultiplication(state: State) {
    let node, last

    node = parseRule2(state)
    last = node

    while (true) {
        if ((this.state.tokenType === TOKENTYPE.SYMBOL) ||
            (this.state.token === 'in' && isConstantNode(node)) ||
            (this.state.tokenType === TOKENTYPE.NUMBER &&
                !isConstantNode(last) &&
                (!isOperatorNode(last) || last.op === '!')) ||
            (this.state.token === '(')) {
            // parse implicit multiplication
            //
            // symbol:      implicit multiplication like '2a', '(2+3)a', 'a b'
            // number:      implicit multiplication like '(2+3)2'
            // parenthesis: implicit multiplication likethis.'2(3+4)', '(3+4)(1+2)'
            last = this.parseRule2()
            node = new OperatorNode('*', 'multiply', [node, last], true /* implicit */)
        } else {
            break
        }
    }

    return node
}