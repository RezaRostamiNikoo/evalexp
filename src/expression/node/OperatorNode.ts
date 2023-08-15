import { map } from "../../utils/array";
import { getSafeProperty, getSafePropertyFromComplexObject, isSafeMethod } from "../../utils/customs";
import { isNode } from "../../utils/is";
import { ExpressionNode } from "./ExpressionNode";

export class OperatorNode extends ExpressionNode {
    name: string = "OperatorNode";
    isPercentage: boolean = false;
    implicit: boolean = false;
    op: string;
    fn: string;
    args: Array<ExpressionNode>;

    get isOperatorNode() { return true }


    /**
    * @constructor OperatorNode
    * @extends {Node}
    * An operator with two arguments, like 2+3
    *
    * @param {string} op           Operator name, for example '+'
    * @param {string} fn           Function name, for example 'add'
    * @param {ExpressionNode[]} args         Operator arguments
    * @param {boolean} [implicit]  Is this an implicit multiplication?
    * @param {boolean} [isPercentage] Is this an percentage Operation?
    */
    constructor(op: string, fn: string, args: Array<ExpressionNode>, implicit: boolean = false, isPercentage: boolean = false) {
        super();

        // validate input
        if (typeof op !== 'string') {
            throw new TypeError('string expected for parameter "op"')
        }
        if (typeof fn !== 'string') {
            throw new TypeError('string expected for parameter "fn"')
        }
        if (!Array.isArray(args) || !args.every(isNode)) {
            throw new TypeError(
                'Array containing Nodes expected for parameter "args"')
        }

        this.implicit = (implicit === true)
        this.isPercentage = (isPercentage === true)
        this.op = op
        this.fn = fn
        this.args = args || []
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
        // validate fn
        if (typeof this.fn !== 'string' || !isSafeMethod(math, this.fn)) {
            console.log("asdasdasdasdasdasdasdasdasd")
            if (!math[this.fn]) {
                throw new Error(
                    'Function ' + this.fn + ' missing in provided namespace "math"')
            } else {
                throw new Error('No access to function "' + this.fn + '"')
            }
        }

        const fn = getSafePropertyFromComplexObject(math, this.fn)
        const evalArgs = map(this.args, function (arg) {
            return arg._compile(math, argNames)
        })

        if (evalArgs.length === 1) {
            const evalArg0 = evalArgs[0]
            return function evalOperatorNode(scope, args, context) {
                return fn(evalArg0(scope, args, context))
            }
        } else if (evalArgs.length === 2) {
            const evalArg0 = evalArgs[0]
            const evalArg1 = evalArgs[1]
            return function evalOperatorNode(scope, args, context) {
                return fn(
                    evalArg0(scope, args, context),
                    evalArg1(scope, args, context))
            }
        } else {
            return function evalOperatorNode(scope, args, context) {
                return fn.apply(null, map(evalArgs, function (evalArg) {
                    return evalArg(scope, args, context)
                }))
            }
        }
    }

    /**
     * Execute a callback for each of the child nodes of this node
     * @param {function(child: Node, path: string, parent: Node)} callback
     */
    forEach(callback) {
        for (let i = 0; i < this.args.length; i++) {
            callback(this.args[i], 'args[' + i + ']', this)
        }
    }

    /**
     * Create a new OperatorNode whose children are the results of calling
     * the provided callback function for each child of the original node.
     * @param {function(child: Node, path: string, parent: Node): Node} callback
     * @returns {OperatorNode} Returns a transformed copy of the node
     */
    map(callback: (child: ExpressionNode, path: string, parent: ExpressionNode) => ExpressionNode) {
        const args = []
        for (let i = 0; i < this.args.length; i++) {
            args[i] = this._ifNode(callback(this.args[i], 'args[' + i + ']', this))
        }
        return new OperatorNode(
            this.op, this.fn, args, this.implicit, this.isPercentage)
    }

    /**
     * Create a clone of this node, a shallow copy
     * @return {OperatorNode}
     */
    clone() {
        return new OperatorNode(
            this.op, this.fn, this.args.slice(0), this.implicit, this.isPercentage)
    }

    /**
     * Check whether this is an unary OperatorNode:
     * has exactly one argument, like `-a`.
     * @return {boolean}
     *     Returns true when an unary operator node, false otherwise.
     */
    isUnary(): boolean {
        return this.args.length === 1
    }

    /**
     * Check whether this is a binary OperatorNode:
     * has exactly two arguments, like `a + b`.
     * @return {boolean}
     *     Returns true when a binary operator node, false otherwise.
     */
    isBinary(): boolean {
        return this.args.length === 2
    }

    /**
     * Get string representation.
     * @param {Object} options
     * @return {string} str
     */
    _toString(options) {
        if (["unaryPlus", "unaryMinus"].includes(this.fn))
            return `${this.op}${this.args[0].toString()}`

        const result = `${this.args[0].toString()} ${this.op} ${this.args[1].toString()}`;
        if (this.implicit) {
            return `(${result})`
        }
        return result;
    }

    /**
     * Get a JSON representation of the node
     * @returns {Object}
     */
    toJSON() {
        return {
            mathjs: this.name,
            op: this.op,
            fn: this.fn,
            args: this.args,
            implicit: this.implicit,
            isPercentage: this.isPercentage
        }
    }

    /**
     * Instantiate an OperatorNode from its JSON representation
     * @param {Object} json
     *     An object structured like
     *     ```
     *     {"mathjs": "OperatorNode",
     *      "op": "+", "fn": "add", "args": [...],
     *      "implicit": false,
     *      "isPercentage":false}
     *     ```
     *     where mathjs is optional
     * @returns {OperatorNode}
     */
    public static fromJSON(json): OperatorNode {
        return new OperatorNode(
            json.op, json.fn, json.args, json.implicit, json.isPercentage)
    }

    /**
     * Get identifier.
     * @return {string}
     */
    getIdentifier() {
        return this.type + ':' + this.fn
    }

}