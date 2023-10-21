import { isAccessorNode, isSymbolNode } from "../../utils/is"
import { createSyntaxError } from "../../errors";
import { AccessorNode } from "../../node/AccessorNode";
import { FunctionNode } from "../../node/FunctionNode";
import { IndexNode } from "../../node/IndexNode";
import { ExpressionNode } from "../../node/ExpressionNode"
import { State } from "../State"
import { parseRelational } from "./parseRelational"
import { SymbolNode } from "../../node/SymbolNode";

/**
 * parse accessors:
 * - function invocation in round brackets (...), for example this.sqrt(2)
 * - index enclosed in square brackets [...], for example A[2,3]
 * - dot notation for properties, like foo.bar
 * @param {State} state
 * @param {ExpressionNode} node    Node on which to apply the parameters. If there
 *                       are no parameters in the expression, the node
 *                       itself is returned
 * @param {string[]} [types]  Filter the types of notations
 *                            can be ['(', '[', '.']
 * @return {ExpressionNode} node
 * @private
 */
export function parseAccessors(state: State, node: ExpressionNode, types?: Array<String>) {
    let params: Array<ExpressionNode>;

    while (['(', '[', '.'].includes(state.token?.value) &&
        (!types || types.indexOf(state.token.value) !== -1)) { // eslint-disable-line no-unmodified-loop-condition
        params = []

        if (state.isToken('(')) {
            if (isSymbolNode(node) || isAccessorNode(node)) {
                // function invocation likethis.fn(2, 3) or obj.fn(2, 3)

                state.goAhead();
                // state.nextLevel(); // for parantheses items inside the parantheses

                if (!state.isToken(')')) {
                    params.push(parseRelational(state))

                    // parse a list with parameters
                    while (state.isToken(',')) { // eslint-disable-line no-unmodified-loop-condition
                        state.goAhead()
                        params.push(parseRelational(state))
                    }
                }

                if (!state.isToken(')')) {
                    throw createSyntaxError(state, 'Parenthesis ) expected')
                }
                // state.prevLevel() // for items
                state.goAhead()

                node = new FunctionNode(node as unknown as SymbolNode, params)
            } else {
                // implicit multiplication like (2+3)(4+5) or this.sqrt(2)(1+2)
                // don't parse it here but let it be handled by parseImplicitMultiplication
                // with correct precedence
                return node
            }
        } else if (state.isToken('[')) {
            // index notation like variable[2, 3]
            state.goAhead();
            // state.nextLevel(); // for items inside the parantheses

            if (!state.isToken(']')) {
                params.push(parseRelational(state))

                // parse a list with parameters
                while (state.isToken(',')) { // eslint-disable-line no-unmodified-loop-condition
                    state.goAhead();
                    params.push(parseRelational(state))
                }
            }

            if (!state.isToken(']')) {
                throw createSyntaxError(state, 'Parenthesis ] expected')
            }
            // state.prevLevel();
            state.goAhead();

            node = new AccessorNode(node, new IndexNode(params))
        } else {
            // dot notation like variable.prop
            state.goAhead();

            if (!state.isType("SYMBOL")) {
                throw createSyntaxError(state, 'Property name expected after dot')
            }
            params.push(new SymbolNode(state.token.value))
            state.goAhead();

            const dotNotation = true
            node = new AccessorNode(node, new IndexNode(params, dotNotation))
        }
    }

    return node;
}