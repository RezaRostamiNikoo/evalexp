import { IScope } from "../interfaces";
import { getSafeProperty } from "../utils/customs";
import { isArray, isConstantNode, isMatrix, isNode, isString, isSymbolNode, typeOf } from "../utils/is";
import { ExpressionNode } from "./ExpressionNode"

export class IndexNode extends ExpressionNode {
    name = "IndexNode";
    dimensions: Array<ExpressionNode>;
    dotNotation: boolean = false;
    get type() { return this.name }
    get isIndexNode(): boolean { return true }


    /** 
     * @constructor IndexNode
     * @extends Node
     *
     * Describes a subset of a matrix or an object property.
     * Cannot be used on its own, needs to be used within an AccessorNode or
     * AssignmentNode.
     *
     * @param {Node[]} dimensions
     * @param {boolean} [dotNotation=false]
     *     Optional property describing whether this index was written using dot
     *     notation like `a.b`, or using bracket notation like `a["b"]`
     *     (which is the default). This property is used for string conversion.
     */
    constructor(dimensions: Array<ExpressionNode>, dotNotation: boolean = false) {
        super()
        this.dimensions = dimensions
        this.dotNotation = dotNotation || false

        // validate input
        if (!Array.isArray(dimensions) || !dimensions.every(isNode)) {
            throw new TypeError(
                'Array containing Nodes expected for parameter "dimensions"')
        }
        if (this.dotNotation && !this.isObjectProperty()) {
            throw new Error('dotNotation only applicable for object properties')
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
        // optimization for a simple object property
        if (this.isObjectProperty() && this.dotNotation) {
            return (scope: IScope) => this.dimensions[0].value
        }
        const evalDimensions = this.dimensions.map(d => d._compile(mathFunctions))

        return (scope: IScope) => {
            return evalDimensions.map(evalDimension => {
                return evalDimension(scope)
            })
        }
    }

    /**
     * Execute a callback for each of the child nodes of this node
     * @param {function(child: Node, path: string, parent: Node)} callback
     */
    forEach(callback) {
        for (let i = 0; i < this.dimensions.length; i++) {
            callback(this.dimensions[i], 'dimensions[' + i + ']', this)
        }
    }

    /**
     * Create a new IndexNode whose children are the results of calling
     * the provided callback function for each child of the original node.
     * @param {function(child: Node, path: string, parent: Node): Node} callback
     * @returns {IndexNode} Returns a transformed copy of the node
     */
    map(callback) {
        const dimensions = []
        for (let i = 0; i < this.dimensions.length; i++) {
            dimensions[i] = this._ifNode(
                callback(this.dimensions[i], 'dimensions[' + i + ']', this))
        }

        return new IndexNode(dimensions, this.dotNotation)
    }

    /**
     * Create a clone of this node, a shallow copy
     * @return {IndexNode}
     */
    clone() {
        return new IndexNode(this.dimensions.slice(0), this.dotNotation)
    }

    /**
     * Test whether this IndexNode contains a single property name
     * @return {boolean}
     */
    isObjectProperty() {
        return this.dimensions.length === 1 &&
            isSymbolNode(this.dimensions[0]) &&
            typeof this.dimensions[0].value === 'string'
    }

    /**
     * Returns the property name if IndexNode contains a property.
     * If not, returns null.
     * @return {string | null}
     */
    getObjectProperty() {
        return this.isObjectProperty() ? this.dimensions[0].value : null
    }

    /**
     * Get string representation
     * @param {Object} options
     * @return {string} str
     */
    _toString(options) {
        // format the parameters like "[1, 0:5]"
        return this.dotNotation
            ? ('.' + this.getObjectProperty())
            : ('[' + this.dimensions.join(', ') + ']')
    }

    /**
     * Get a JSON representation of the node
     * @returns {Object}
     */
    toJSON() {
        return {
            mathjs: this.name,
            dimensions: this.dimensions,
            dotNotation: this.dotNotation
        }
    }

    /**
     * Instantiate an IndexNode from its JSON representation
     * @param {Object} json
     *     An object structured like
     *     `{"mathjs": "IndexNode", dimensions: [...], dotNotation: false}`,
     *     where mathjs is optional
     * @returns {IndexNode}
     */
    static fromJSON(json) {
        return new IndexNode(json.dimensions, json.dotNotation)
    }

}