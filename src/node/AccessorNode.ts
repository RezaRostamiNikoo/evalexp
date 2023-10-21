import { IndexNode } from "./IndexNode";
import { ExpressionNode } from "./ExpressionNode";
import { isIndexNode, isNode } from "../utils/is";
import { getSafeProperty } from "../utils/customs";
import { IScope } from "../interfaces";


// const access = accessFactory({ subset })

// /**
//  * Are parenthesis needed?
//  * @private
//  */
// function needParenthesis(node) {
//     // TODO: maybe make a method on the nodes which tells whether they need parenthesis?
//     return !(
//         isAccessorNode(node) ||
//         isArrayNode(node) ||
//         isConstantNode(node) ||
//         isFunctionNode(node) ||
//         isObjectNode(node) ||
//         isParenthesisNode(node) ||
//         isSymbolNode(node))
// }
export class AccessorNode extends ExpressionNode {
    _name: string = "AccessorNode";
    object: ExpressionNode; // TODO: it should be an object if it can be defined in a expression but it can be a symbolNode because it shoud be defined in scope
    index: IndexNode;
    get type() { return this._name }
    get isAccessorNode(): boolean { return true }
    // readonly property name
    get name() {
        if (this.index) {
            return (this.index.isObjectProperty())
                ? this.index.getObjectProperty()
                : ''
        } else {
            return this.object._name || ''
        }
    }
    /**
     * @constructor AccessorNode
     * @extends {ExpressionNode}
     * Access an object property or get a matrix subset
     *
     * @param {ExpressionNode} object                 The object from which to retrieve
     *                                      a property or subset.
     * @param {IndexNode} index             IndexNode containing ranges
     */
    constructor(object: ExpressionNode, index: IndexNode) {
        super()
        if (!isNode(object)) {
            throw new TypeError('Node expected for parameter "object"')
        }
        if (!isIndexNode(index)) {
            throw new TypeError('IndexNode expected for parameter "index"')
        }

        this.object = object
        this.index = index
    }

    /**
     * Compile a node into a JavaScript function.
     * This basically pre-calculates as much as possible and only leaves open
     * calculations which depend on a dynamic scope with variables.
     * @param {Object} mathFunctions namespace with functions and constants.
     * @return {(scope: IScope): any} Returns a function which can be called like: evalNode(scope: Object)
     */
    _compile(mathFunctions: Object): (scope: IScope) => any {
        const evalObject = this.object._compile(mathFunctions)
        const evalIndex = this.index._compile(mathFunctions)

        if (this.index.isObjectProperty()) {
            const prop = this.index.getObjectProperty()
            return (scope: IScope) => {
                // get a property from an object evaluated using the scope.
                return getSafeProperty(evalObject(scope), prop)
            }
        } else {
            return (scope: IScope) => {
                let object = evalObject(scope)
                // we pass just object here instead of context:
                const indexes = evalIndex(scope)
                indexes.forEach(i => object = object[i])
                return object
            }
        }
    }

    /**
     * Execute a callback for each of the child nodes of this node
     * @param {function(child: Node, path: string, parent: Node)} callback
     */
    forEach(callback) {
        callback(this.object, 'object', this)
        callback(this.index, 'index', this)
    }

    /**
     * Create a new AccessorNode whose children are the results of calling
     * the provided callback function for each child of the original node.
     * @param {function(child: Node, path: string, parent: Node): Node} callback
     * @returns {AccessorNode} Returns a transformed copy of the node
     */
    map(callback) {
        return new AccessorNode(
            this._ifNode(callback(this.object, 'object', this)),
            this._ifNode(callback(this.index, 'index', this))
        )
    }

    /**
     * Create a clone of this node, a shallow copy
     * @return {AccessorNode}
     */
    clone() {
        return new AccessorNode(this.object, this.index)
    }

    /**
     * Get string representation
     * @param {Object} options
     * @return {string}
     */
    _toString(options) {
        return `${this.object.toString(options)}${this.index.toString(options)}`;
    }

    /**
     * Instantiate an AccessorNode from its JSON representation
     * @param {Object} json
     *     An object structured like
     *     `{"mathjs": "AccessorNode", object: ..., index: ...}`,
     *     where mathjs is optional
     * @returns {AccessorNode}
     */
    static fromJSON(json) {
        return new AccessorNode(json.object, json.index)
    }
}