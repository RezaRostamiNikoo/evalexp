import { getSafeMethod, getSafeProperty, getSafePropertyFromComplexObject } from "../utils/customs";
import { isAccessorNode, isFunctionAssignmentNode, isIndexNode, isNode, isSymbolNode } from "../utils/is";
import { hasOwnProperty } from "../utils/object";
import { createSubScope } from "../utils/scope";
import { format } from "../utils/string";
import { ExpressionNode } from "./ExpressionNode";
import { SymbolNode } from "./SymbolNode";

/* format to fixed length */
const strin = entity => format(entity, { truncate: 78 })

/*
 * Expand a LaTeX template
 *
 * @param {string} template
 * @param {Node} node
 * @param {Object} options
 * @private
 **/
function expandTemplate(template, node, options) {
    let latex = ''

    // Match everything of the form ${identifier} or ${identifier[2]} or $$
    // while submatching identifier and 2 (in the second case)
    const regex = /\$(?:\{([a-z_][a-z_0-9]*)(?:\[([0-9]+)\])?\}|\$)/gi

    let inputPos = 0 // position in the input string
    let match
    while ((match = regex.exec(template)) !== null) { // go through all matches
        // add everything in front of the match to the LaTeX string
        latex += template.substring(inputPos, match.index)
        inputPos = match.index

        if (match[0] === '$$') { // escaped dollar sign
            latex += '$'
            inputPos++
        } else { // template parameter
            inputPos += match[0].length
            const property = node[match[1]]
            if (!property) {
                throw new ReferenceError('Template: Property ' + match[1] + ' does not exist.')
            }
            if (match[2] === undefined) { // no square brackets
                switch (typeof property) {
                    case 'string':
                        latex += property
                        break
                    case 'object':
                        if (isNode(property)) {
                            latex += property.toTex(options)
                        } else if (Array.isArray(property)) {
                            // make array of Nodes into comma separated list
                            latex += property.map(function (arg, index) {
                                if (isNode(arg)) {
                                    return arg.toTex(options)
                                }
                                throw new TypeError('Template: ' + match[1] + '[' + index + '] is not a Node.')
                            }).join(',')
                        } else {
                            throw new TypeError('Template: ' + match[1] + ' has to be a Node, String or array of Nodes')
                        }
                        break
                    default:
                        throw new TypeError('Template: ' + match[1] + ' has to be a Node, String or array of Nodes')
                }
            } else { // with square brackets
                if (isNode(property[match[2]] && property[match[2]])) {
                    latex += property[match[2]].toTex(options)
                } else {
                    throw new TypeError('Template: ' + match[1] + '[' + match[2] + '] is not a Node.')
                }
            }
        }
    }
    latex += template.slice(inputPos) // append rest of the template

    return latex
}

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
        // compile arguments
        const evalArgs = this.args.map((arg) => arg._compile(math, argNames))

        if (isSymbolNode(this.fn)) {
            const name = this.fn.name
            if (!argNames[name]) {
                // we can statically determine whether the function
                // has the rawArgs property
                const fn = name in math ? getSafePropertyFromComplexObject(math, name) : undefined
                const isRaw = typeof fn === 'function' && fn.rawArgs === true

                const resolveFn = (scope) => {
                    let value
                    if (scope.has(name)) {
                        value = scope.get(name)
                    } else if (name in math) {
                        value = getSafePropertyFromComplexObject(math, name)
                    } else {
                        return FunctionNode.onUndefinedFunction(name)
                    }
                    if (typeof value === 'function') {
                        return value
                    }
                    throw new TypeError(
                        `'${name}' is not a function; its value is:\n  ${strin(value)}`
                    )
                }

                if (isRaw) {
                    // pass unevaluated parameters (nodes) to the function
                    // "raw" evaluation
                    const rawArgs = this.args
                    return function evalFunctionNode(scope, args, context) {
                        const fn = resolveFn(scope)
                        return fn(rawArgs, math, createSubScope(scope, args), scope)
                    }
                } else {
                    // "regular" evaluation
                    switch (evalArgs.length) {
                        case 0: return function evalFunctionNode(scope, args, context) {
                            const fn = resolveFn(scope)
                            return fn()
                        }
                        case 1: return function evalFunctionNode(scope, args, context) {
                            const fn = resolveFn(scope)
                            const evalArg0 = evalArgs[0]
                            return fn(
                                evalArg0(scope, args, context)
                            )
                        }
                        case 2: return function evalFunctionNode(scope, args, context) {
                            const fn = resolveFn(scope)
                            const evalArg0 = evalArgs[0]
                            const evalArg1 = evalArgs[1]
                            return fn(
                                evalArg0(scope, args, context),
                                evalArg1(scope, args, context)
                            )
                        }
                        default: return function evalFunctionNode(scope, args, context) {
                            const fn = resolveFn(scope)
                            const values = evalArgs.map(
                                (evalArg) => evalArg(scope, args, context))
                            return fn(...values)
                        }
                    }
                }
            } else { // the function symbol is an argName
                const rawArgs = this.args
                return function evalFunctionNode(scope, args, context) {
                    const fn = getSafeProperty(args, name)
                    if (typeof fn !== 'function') {
                        throw new TypeError(
                            `Argument '${name}' was not a function; received: ${strin(fn)}`
                        )
                    }
                    if (fn.rawArgs) {
                        // "Raw" evaluation
                        return fn(rawArgs, math, createSubScope(scope, args), scope)
                    } else {
                        const values = evalArgs.map(
                            (evalArg) => evalArg(scope, args, context))
                        return fn.apply(fn, values)
                    }
                }
            }
        } else if (1 == 1
            // isAccessorNode(this.fn) &&
            // isIndexNode(this.fn.index) &&
            // this.fn.index.isObjectProperty()
        ) {
            // execute the function with the right context:
            // the object of the AccessorNode

            // const evalObject = this.fn.object._compile(math, argNames)
            // const prop = this.fn.index.getObjectProperty()
            // const rawArgs = this.args

            // return function evalFunctionNode(scope, args, context) {
            //     const object = evalObject(scope, args, context)
            //     const fn = getSafeMethod(object, prop)

            //     if (fn?.rawArgs) {
            //         // "Raw" evaluation
            //         return fn(rawArgs, math, createSubScope(scope, args), scope)
            //     } else {
            //         // "regular" evaluation
            //         const values = evalArgs.map((evalArg) => evalArg(scope, args, context))
            //         return fn.apply(object, values)
            //     }
            // }
        } else {
            // node.fn.isAccessorNode && !node.fn.index.isObjectProperty()
            // we have to dynamically determine whether the function has the
            // rawArgs property
            const fnExpr = this.fn.toString()
            const evalFn = this.fn._compile(math, argNames)
            const rawArgs = this.args

            return function evalFunctionNode(scope, args, context) {
                const fn = evalFn(scope, args, context)
                if (typeof fn !== 'function') {
                    throw new TypeError(
                        `Expression '${fnExpr}' did not evaluate to a function; value is:` +
                        `\n  ${strin(fn)}`
                    )
                }
                if (fn.rawArgs) {
                    // "Raw" evaluation
                    return fn(rawArgs, math, createSubScope(scope, args), scope)
                } else {
                    // "regular" evaluation
                    const values = evalArgs.map(
                        (evalArg) => evalArg(scope, args, context))
                    return fn.apply(fn, values)
                }
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