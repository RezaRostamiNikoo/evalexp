import { IScope } from "../interfaces";
import { getSafePropertyFromComplexObject } from "../utils/customs";
import { isFunctionAssignmentNode, isIndexNode, isNode, isSymbolNode } from "../utils/is";
import { ExpressionNode } from "./ExpressionNode";
import { SymbolNode } from "./SymbolNode";


export class FunctionNode extends ExpressionNode {
    _name: string = "FunctionNode";
    fn: SymbolNode;
    args: Array<ExpressionNode>;

    get type(): string { return this.name }
    get isFunctionNode(): boolean { return true }
    // readonly property name
    get name(): string {
        return this.fn.name || ''
    }

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
        if (typeof fn === 'string') fn = new SymbolNode(fn)
        if (!isSymbolNode(fn)) throw new TypeError('Node expected as parameter "fn"')

        if (!Array.isArray(args) || !args.every(isNode))
            throw new TypeError('Array containing Nodes expected for parameter "args"')

        this.fn = fn
        this.args = args || []
    }


    /**
     * Compile a node into a JavaScript function.
     * This basically pre-calculates as much as possible and only leaves open
     * calculations which depend on a dynamic scope with variables.
     * @param {Object} math     Math.js namespace with functions and constants.
     * @return {(scope: IScope): any} Returns a function which can be called like: evalNode(scope: Object)

     */
    _compile(mathFunctions: Object): (scope: IScope) => any {
        // compile arguments
        const evalArgs = this.args.map((arg) => arg._compile(mathFunctions))

        const name = this.fn.name
        const fn = name in mathFunctions ? getSafePropertyFromComplexObject(mathFunctions, name) : undefined
        const isRaw = typeof fn === 'function' && fn.rawArgs === true

        const resolveFn = (scope) => {
            let value
            if (name in mathFunctions) {
                value = getSafePropertyFromComplexObject(mathFunctions, name)
            } else if (scope.has(name)) {
                value = scope.get(name)
            } else {
                throw new Error("FunctionNode._compile | there is no functino defined")
            }
            if (typeof value === 'function') {
                return value
            }
            throw new TypeError(
                `'${name}' is not a function; its value is:\n  ${value}`
            )
        }

        // "regular" evaluation
        switch (evalArgs.length) {
            case 0: return (scope: IScope): any => {
                const fn = resolveFn(scope)
                return fn()
            }
            case 1: return (scope: IScope): any => {
                const fn = resolveFn(scope)
                const evalArg0 = evalArgs[0]
                return fn(evalArg0(scope))
            }
            case 2: return (scope: IScope): any => {
                const fn = resolveFn(scope)
                const evalArg0 = evalArgs[0]
                const evalArg1 = evalArgs[1]
                return fn(evalArg0(scope), evalArg1(scope))
            }
            default: return (scope: IScope): any => {
                const fn = resolveFn(scope)
                return fn.apply(null, evalArgs.map(evalArg => evalArg(scope)))
            }
        }
    }

    /**
     * Execute a callback for each of the child nodes of this node
     * @param {function(child: Node, path: string, parent: Node)} callback
     */
    forEach(callback) {
        callback(this.fn, 'fn', this)

        for (let i = 0; i < this.args.length; i++) {
            callback(this.args[i], 'args[' + i + ']', this)
        }
    }

    /**
     * Create a new FunctionNode whose children are the results of calling
     * the provided callback function for each child of the original node.
     * @param {function(child: Node, path: string, parent: Node): Node} callback
     * @returns {FunctionNode} Returns a transformed copy of the node
     */
    map(callback: (child: ExpressionNode, path: string, parent: ExpressionNode) => ExpressionNode): FunctionNode {
        const fn = this._ifNode(callback(this.fn, 'fn', this))
        const args = []
        for (let i = 0; i < this.args.length; i++) {
            args[i] = this._ifNode(callback(this.args[i], 'args[' + i + ']', this))
        }
        return new FunctionNode(fn, args);;
    }

    /**
     * Create a clone of this node, a shallow copy
     * @return {FunctionNode}
     */
    clone() {
        return new FunctionNode(this.fn, this.args.slice(0))
    }

    /**
     * Throws an error 'Undefined function {name}'
     * @param {string} name
     */
    static onUndefinedFunction = function (name) {
        throw new Error('Undefined function ' + name)
    }

    /**
     * Get string representation. (wrapper function)
     * This overrides parts of Node's toString function.
     * If callback is an object containing callbacks, it
     * calls the correct callback for the current node,
     * otherwise it falls back to calling Node's toString
     * function.
     *
     * @param {Object} options
     * @return {string} str
     * @override
     */
    toString(options) {
        return `${this.fn}(${this.args.map(a => a.toString()).join(", ")})`;

        // let customString
        // const name = this.fn.toString(options)
        // if (options &&
        //     (typeof options.handler === 'object') &&
        //     hasOwnProperty(options.handler, name)) {
        //     // callback is a map of callback functions
        //     customString = options.handler[name](this, options)
        // }

        // if (typeof customString !== 'undefined') {
        //     return customString
        // }

        // // fall back to Node's toString
        // return super.toString(options)
    }
    /**
     * Get string representation
     * @param {Object} options
     * @return {string} str
     */
    _toString(options) {
        const args = this.args.map(function (arg) {
            return arg.toString(options)
        })

        const fn = isFunctionAssignmentNode(this.fn)
            ? ('(' + this.fn.toString(options) + ')')
            : this.fn.toString(options)

        // format the arguments like "add(2, 4.2)"
        return fn + '(' + args.join(', ') + ')'
    }

    /**
     * Get a JSON representation of the node
     * @returns {Object}
     */
    toJSON() {
        return {
            mathjs: this.name,
            fn: this.fn,
            args: this.args
        }
    }

    /**
     * Instantiate an AssignmentNode from its JSON representation
     * @param {Object} json  An object structured like
     *                       `{"mathjs": "FunctionNode", fn: ..., args: ...}`,
     *                       where mathjs is optional
     * @returns {FunctionNode}
     */
    static fromJSON = function (json) {
        return new FunctionNode(json.fn, json.args)
    }

    /**
     * Get identifier.
     * @return {string}
     */
    getIdentifier() {
        return this.type + ':' + this.name
    }


}