import { isNode } from "../utils/is";
import { deepStrictEqual } from "../utils/object";
import * as functions from "../defaultScopeItems";
import { parse } from "../parser";
import { IScope } from "../interfaces/IScope";

export abstract class ExpressionNode {

    _name: string = "ExpressionNode";
    get type() { return 'Node' }
    get isNode() { return true }

    value: string;

    /**
     * Evaluate the node
     * @param {Scope} [scope] Scope to read/write variables
     * @return {*} Returns the result
     */
    evaluate(scope: IScope = null): Object | string | number {
        return this.compile().evaluate(scope);
    }

    /**
     * Compile the node into an optimized, evauatable JavaScript function
     * @returns {{evaluate: function([Object])}} object
     * Returns an object with a function 'evaluate',
     * which can be invoked as expr.evaluate([scope: Object]),
     * where scope is an optional object with
     * variables.
     */
    compile(): { evaluate: (scope: IScope) => any } {
        const expr = this._compile(functions)
        return {
            evaluate: (scope: IScope) => {
                return expr(scope)
            }
        }
    }

    /**
     * Compile a node into a JavaScript function.
     * This basically pre-calculates as much as possible and only leaves open
     * calculations which depend on a dynamic scope with variables.
     * @param {Object} mathFunctions namespace with functions and constants.
     * @return {(scope: IScope): any} Returns a function which can be called like: evalNode(scope: Object)
     */
    _compile(mathFunctions: Object): (scope: IScope) => any {
        throw new Error('Method _compile must be implemented by type ' + this.type)
    }

    /**
     * Execute a callback for each of the child nodes of this node
     * @param {function(child: Node, path: string, parent: Node)} callback
     */
    forEach(callback: (child: ExpressionNode, path: string, parent: ExpressionNode) => void) {
        // must be implemented by each of the Node implementations
        throw new Error('Cannot run forEach on a ExpressionNode interface')
    }


    /**
     * Create a new Node whose children are the results of calling the
     * provided callback function for each child of the original node.
     * @param {function(child: Node, path: string, parent: Node): Node} callback
     * @returns {OperatorNode} Returns a transformed copy of the node
     */
    map(callback: (child: ExpressionNode, path: string, parent: ExpressionNode) => ExpressionNode): ExpressionNode {
        // must be implemented by each of the Node implementations
        throw new Error('Cannot run map on a Node interface')
    }

    /**
     * Validate whether an object is a Node, for use with map
     * @param {Node} node
     * @returns {Node} Returns the input if it's a node, else throws an Error
     * @protected
     */
    _ifNode(node) {
        if (!isNode(node)) {
            throw new TypeError('Callback function must return a ExpressionNode')
        }
        return node
    }

    /**
     * Recursively traverse all nodes in a node tree. Executes given callback for
     * this node and each of its child nodes.
     * @param {function(node: Node, path: string, parent: Node)} callback
     *          A callback called for every node in the node tree.
     */
    traverse(callback) {
        // execute callback for itself
        // eslint-disable-next-line
        callback(this, null, null)

        // recursively traverse over all children of a node
        function _traverse(node, callback) {
            node.forEach(function (child, path, parent) {
                callback(child, path, parent)
                _traverse(child, callback)
            })
        }

        _traverse(this, callback)
    }

    /**
     * Recursively transform a node tree via a transform function.
     *
     * For example, to replace all nodes of type SymbolNode having name 'x' with
     * a ConstantNode with value 2:
     *
     *     const res = Node.transform(function (node, path, parent) {
     *       if (node && node.isSymbolNode) && (node.name === 'x')) {
     *         return new ConstantNode(2)
     *       }
     *       else {
     *         return node
     *       }
     *     })
     *
     * @param {function(node: Node, path: string, parent: Node) : Node} callback
     *          A mapping function accepting a node, and returning
     *          a replacement for the node or the original node. The "signature"
     *          of the callback must be:
     *          callback(node: Node, index: string, parent: Node) : Node
     * @return {Node} Returns the original node or its replacement
     */
    transform(callback) {
        function _transform(child, path, parent) {
            const replacement = callback(child, path, parent)

            if (replacement !== child) {
                // stop iterating when the node is replaced
                return replacement
            }

            return child.map(_transform)
        }

        return _transform(this, null, null)
    }

    /**
     * Find any node in the node tree matching given filter function. For
     * example, to find all nodes of type SymbolNode having name 'x':
     *
     *     const results = Node.filter(function (node) {
     *       return (node && node.isSymbolNode) && (node.name === 'x')
     *     })
     *
     * @param {function(node: Node, path: string, parent: Node) : Node} callback
     *            A test function returning true when a node matches, and false
     *            otherwise. Function signature:
     *            callback(node: Node, index: string, parent: Node) : boolean
     * @return {Node[]} nodes
     *            An array with nodes matching given filter criteria
     */
    filter(callback) {
        const nodes = []

        this.traverse(function (node, path, parent) {
            if (callback(node, path, parent)) {
                nodes.push(node)
            }
        })

        return nodes
    }

    /**
     * Create a shallow clone of this node
     * @return {Node}
     */
    clone() {
        // must be implemented by each of the Node implementations
        throw new Error('Cannot clone a Node interface')
    }

    /**
     * Create a deep clone of this node
     * @return {Node}
     */
    cloneDeep() {
        return this.map(function (node) {
            return node.cloneDeep()
        })
    }

    /**
     * Deep compare this node with another node.
     * @param {Node} other
     * @return {boolean} Returns true when both nodes are of the same type and
     *                   contain the same values (as do their childs)
     */
    equals(other) {
        return other
            ? this.type === other.type && deepStrictEqual(this as unknown as Object, other)
            : false
    }

    /**
     * Get string representation. (wrapper function)
     *
     * This function can get an object of the following form:
     * {
     *    handler: //This can be a callback function of the form
     *             // "function callback(node, options)"or
     *             // a map that maps function names (used in FunctionNodes)
     *             // to callbacks
     *    parenthesis: "keep" //the parenthesis option (This is optional)
     * }
     *
     * @param {Object} [options]
     * @return {string}
     */
    toString(options?: Object) {
        const customString = this._getCustomString(options)

        if (typeof customString !== 'undefined') {
            return customString
        }

        return this._toString(options)
    }


    /**
     * Get a JSON representation of the node
     * Both .toJSON() and the static .fromJSON(json) should be implemented by all
     * implementations of Node
     * @returns {Object}
     */
    toJSON() {
        throw new Error(
            'Cannot serialize object: toJSON not implemented by ' + this.type)
    }

    /**
     * Internal function to generate the string output.
     * This has to be implemented by every Node
     * @param {Object} options
     * @throws {Error}
     */
    _toString(options: Object) {
        // must be implemented by each of the Node implementations
        throw new Error('_toString not implemented for ' + this.type)
    }

    /**
     * Helper used by `to...` functions.
     */
    _getCustomString(options) {
        if (options && typeof options === 'object') {
            switch (typeof options.handler) {
                case 'object':
                case 'undefined':
                    return
                case 'function':
                    return options.handler(this, options)
                default:
                    throw new TypeError('Object or function expected as callback')
            }
        }
    }

    /**
     * Get identifier.
     * @return {string}
     */
    getIdentifier() {
        return this.type
    }

    /**
     * Get the content of the current Node.
     * @return {Node} node
     **/
    getContent(): ExpressionNode {
        return this;
    }

    protected calculateValue(scope: IScope, name: string): any {
        const item = scope.getItem(name)
        if (!item) return undefined
        if (item.isCalculated()) return item.getCalculatedValue()
        if (typeof item.getRawValue() === "string")
            item.setCalculatedValue(parse(item.getRawValue()).evaluate(scope))
        else item.setCalculatedValue(item.getRawValue())
        return item.getCalculatedValue()
    }
}