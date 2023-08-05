import { parseRelational } from "./parseRelational"

/**
  * parentheses
  * @return {Node} node
  * @private
  */
export function parseParentheses() {
    let node

    // check if it is a parenthesized expression
    if (this.state.token === '(') {
        // parentheses (...)
        this.openParams()
        this.getToken()

        node = parseRelational(state) // start again

        if (!this.isToken(')')) {
            throw this.createSyntaxError('Parenthesis ) expected')
        }
        this.closeParams()
        this.getToken()

        node = new ParenthesisNode(node)
        node = this.parseAccessors(node)
        return node
    }

    return this.parseEnd()
}