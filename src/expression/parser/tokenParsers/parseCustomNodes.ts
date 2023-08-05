import { TOKENTYPE } from "../../constants"
import { State } from "../State"
import { parseRelational } from "./parseRelational"
import { parseSymbol } from "./parseSymbol"

/**
 * Parse a custom node handler. A node handler can be used to process
 * nodes in a custom way, for example for handling a plot.
 *
 * A handler must be passed as second argument of the parse function.
 * - must extend math.Node
 * - must contain a functionthis._compile(defs: Object) : string
 * - must contain a functionthis.find(filter: Object) : Node[]
 * - must contain a functionthis.toString() : string
 * - the constructor is called with a single argument containing all parameters
 *
 * For example:
 *
 *     nodes = {
 *       'plot': PlotHandler
 *     }
 *
 * The constructor of the handler is called as:
 *
 *     node = new PlotHandler(params)
 *
 * The handler will be invoked when evaluating an expression like:
 *
 *     node = math.parse('plot(sin(x), x)', nodes)
 *
 * @return {Node} node
 * @private
 */
export function parseCustomNodes(state:State) {
    let params = []

    if (this.state.tokenType === TOKENTYPE.SYMBOL && this.hasOwnProperty(this.state.extraNodes, this.state.token)) {
        const CustomNode = this.state.extraNodes[this.state.token]

        this.getToken()

        // parse parameters
        if (this.state.token === '(') {
            params = []

            this.openParams()
            this.getToken()

            if (!this.isToken(')')) {
                params.push(parseRelational(state))

                // parse a list with parameters
                while (this.isToken(',')) { // eslint-disable-line no-unmodified-loop-condition
                    this.getToken()
                    params.push(parseRelational(state))
                }
            }

            if (!this.isToken(')')) {
                throw this.createSyntaxError('Parenthesis ) expected')
            }
            this.closeParams()
            state.getToken()
        }

        // create a new custom node
        // noinspection JSValidateTypes
        return new CustomNode(params)
    }

    return  parseSymbol(state)
}
