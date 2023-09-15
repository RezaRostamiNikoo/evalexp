import { map } from "../utils/array";
import { isNode } from "../utils/is";
import { ExpressionNode } from "./ExpressionNode";

export class ArrayNode extends ExpressionNode {
    name = 'ArrayNode';
    items: Array<ExpressionNode>;
    get type(): string { return this.name }
    get isArrayNode(): boolean { return true }


    /**
     * @constructor ArrayNode
     * @extends {ExpressionNode}
     * Holds an 1-dimensional array with items
     * @param {ExpressionNode[]} [items]   1 dimensional array with items
     */
    constructor(items: Array<ExpressionNode>) {
        super()
        this.items = items || []

        // validate input
        if (!Array.isArray(this.items) || !this.items.every(isNode)) {
            throw new TypeError('Array containing Nodes expected')
        }
    }


    /**
     * Compile a node into a JavaScript function.
     * This basically pre-calculates as much as possible and only leaves open
     * calculations which depend on a dynamic scope with variables.
     * @param {Object} math     Math.js namespace with functions and constants.
     * @param {Object} argNames An object with argument names as key and `true`
     *                          as value. Used in the SymbolNode to optimize
     *                          for arguments from user assigned functions
     *                          (see FunctionAssignmentNode) or special symbols
     *                          like `end` (see IndexNode).
     * @return {function} Returns a function which can be called like:
     *                        evalNode(scope: Object, args: Object, context: *)
     */
    _compile(math, argNames) {
        const evalItems = map(this.items, function (item) {
            return item._compile(math, argNames)
        })

        const asMatrix = (math.config.matrix !== 'Array')
        if (asMatrix) {
            const matrix = math.matrix
            return function evalArrayNode(scope, args, context) {
                return matrix(map(evalItems, function (evalItem) {
                    return evalItem(scope, args, context)
                }))
            }
        } else {
            return function evalArrayNode(scope, args, context) {
                return map(evalItems, function (evalItem) {
                    return evalItem(scope, args, context)
                })
            }
        }
    }

    /**
     * Execute a callback for each of the child nodes of this node
     * @param {function(child: Node, path: string, parent: Node)} callback
     */
    forEach(callback) {
        for (let i = 0; i < this.items.length; i++) {
            const node = this.items[i]
            callback(node, 'items[' + i + ']', this)
        }
    }

    /**
     * Create a new ArrayNode whose children are the results of calling
     * the provided callback function for each child of the original node.
     * @param {function(child: Node, path: string, parent: Node): Node} callback
     * @returns {ArrayNode} Returns a transformed copy of the node
     */
    map(callback) {
        const items = []
        for (let i = 0; i < this.items.length; i++) {
            items[i] = this._ifNode(callback(this.items[i], 'items[' + i + ']', this))
        }
        return new ArrayNode(items)
    }

    /**
     * Create a clone of this node, a shallow copy
     * @return {ArrayNode}
     */
    clone() {
        return new ArrayNode(this.items.slice(0))
    }

    /**
     * Get string representation
     * @param {Object} options
     * @return {string} str
     * @override
     */
    _toString(options) {
        return `[${this.items.map(a => a.toString()).join(", ")}]`
    }

    /**
     * Get a JSON representation of the node
     * @returns {Object}
     */
    toJSON() {
        return {
            mathjs: this.name,
            items: this.items
        }
    }

    /**
     * Instantiate an ArrayNode from its JSON representation
     * @param {Object} json  An object structured like
     *                       `{"mathjs": "ArrayNode", items: [...]}`,
     *                       where mathjs is optional
     * @returns {ArrayNode}
     */
    static fromJSON(json) {
        return new ArrayNode(json.items)
    }

}