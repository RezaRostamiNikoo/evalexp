import { ExpressionNode } from "../../node/ExpressionNode"
import { OperatorNode } from "../../node/OperatorNode"
import { State } from "../State"
import { parseSymbol } from "./parseSymbol"
import { parseUnary } from "./parseUnary"

/**
  * power
  * Note: power operator is right associative
  * @return {ExpressionNode} node
  * @private
  */
export function parsePow(state: State): ExpressionNode {
    let node, name, fn, params

    node = parseSymbol(state)

    if (state.isToken('^')) {
        name = state.token.Value
        fn = 'pow'

        state.goAHead();
        params = [node, parseUnary(state)] // Go back to unary, we can have '2^-3'
        node = new OperatorNode(name, fn, params)
    }

    return node;
}