import { isNode } from "../../utils/is";
import { ExpressionNode } from "./ExpressionNode";
import { SymbolNode } from "./SymbolNode";

export class FunctionNode extends ExpressionNode {
    isFunctionNode: boolean = true;
    fn: SymbolNode;
    args: Array<ExpressionNode>;
    /**
     * @constructor FunctionNode
     * @extends {ExpressionNode}
     * invoke a list with arguments on a node
     * @param {ExpressionNode | string} fn
     *     Item resolving to a function on which to invoke
     *     the arguments, typically a SymboNode or AccessorNode
     * @param {ExpressionNode[]} args
     */
    constructor(fn: SymbolNode | string, args: Array<ExpressionNode>) {
        super()
        if (typeof fn === 'string') {
            fn = new SymbolNode(fn)
        }

        // validate input
        if (!isNode(fn)) throw new TypeError('Node expected as parameter "fn"')
        if (!Array.isArray(args) || !args.every(isNode)) {
            throw new TypeError(
                'Array containing Nodes expected for parameter "args"')
        }

        this.fn = fn
        this.args = args || []
    }

    toString(): string {
        return `${this.fn}(${this.args.map(a => a.toString()).join(", ")})`;
    }
}