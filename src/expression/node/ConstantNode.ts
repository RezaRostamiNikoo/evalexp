import { format } from "../../utils/string";
import { ExpressionNode } from "./ExpressionNode";

export class ConstantNode extends ExpressionNode {
    name: string = 'ConstantNode';
    value: any;

    get type() { return this.name }
    get isConstantNode(): boolean { return true }


    /**
     * A ConstantNode holds a constant value like a number or string.
     *
     * Usage:
     *
     *     new ConstantNode(2.3)
     *     new ConstantNode('hello')
     *
     * @param {any} value    Value can be any type (number, BigNumber, string, ...)
     * @constructor ConstantNode
     * @extends {ExpressionNode}
     */
    constructor(value: any) {
        super();
        this.value = value
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
        const value = this.value

        return function evalConstantNode() {
            return parseFloat(value);
        }
    }

    /**
     * Execute a callback for each of the child nodes of this node
     * @param {function(child: Node, path: string, parent: Node)} callback
     */
    forEach(callback) {
        // nothing to do, we don't have any children
    }

    /**
     * Create a new ConstantNode with children produced by the given callback.
     * Trivial because there are no children.
     * @param {function(child: Node, path: string, parent: Node) : Node} callback
     * @returns {ConstantNode} Returns a clone of the node
     */
    map(callback) {
        return this.clone()
    }

    /**
     * Create a clone of this node, a shallow copy
     * @return {ConstantNode}
     */
    clone() {
        return new ConstantNode(this.value)
    }

    /**
     * Get string representation
     * @param {Object} options
     * @return {string} str
     */
    _toString(options) {
        return this.value;
        // return format(this.value, options)
    }

    /**
     * Get a JSON representation of the node
     * @returns {Object}
     */
    toJSON() { return { mathjs: this.name, value: this.value } }

    /**
     * Instantiate a ConstantNode from its JSON representation
     * @param {Object} json  An object structured like
     *                       `{"mathjs": "SymbolNode", value: 2.3}`,
     *                       where mathjs is optional
     * @returns {ConstantNode}
     */
    public static fromJSON(json) {
        return new ConstantNode(json.value)
    }


}