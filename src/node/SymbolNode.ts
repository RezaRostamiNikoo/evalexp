import { IScope } from "../interfaces/IScope";
import { getSafePropertyFromComplexObject } from "../utils/customs";
import { ExpressionNode } from "./ExpressionNode";

export class SymbolNode extends ExpressionNode {
    name: string;

    get type() { return 'SymbolNode' }
    get isSymbolNode() { return true }

    /**
     * @constructor SymbolNode
     * @extends {ExpressionNode}
     * A symbol node can hold and resolve a symbol
     * @param {string} name
     * @extends {ExpressionNode}
     */
    constructor(name: string) {
        super();
        // validate input
        if (typeof name !== 'string') {
            throw new TypeError('String expected for parameter "name"')
        }
        this.value = name
        this.name = name
    }



    /**
     * Compile a node into a JavaScript function.
     * This basically pre-calculates as much as possible and only leaves open
     * calculations which depend on a dynamic scope with variables.
     * @param {Object} scope     Math.js namespace with functions and constants.
     * @return {(scope: IScope): any} Returns a function which can be called like: evalNode(scope: Object)
     */
    _compile(mathFunctions: Object): (scope: IScope) => any {
        const _this = this
        const name = this.name

        if (name in mathFunctions) return (scope: IScope) => {
            return scope.has(name)
                ? scope.getValue(name)
                : getSafePropertyFromComplexObject(mathFunctions, name)
        }
        else return (scope: IScope) => {
            return scope.has(name)
                ? scope.getValue(name)
                : name;
        }
    }


    forEach(callback: (child: ExpressionNode, path: string, parent: ExpressionNode) => void) { }

    /**
     * Create a new SymbolNode with children produced by the given callback.
     * Trivial since a SymbolNode has no children
     * @param {function(child: ExpressionNode, path: string, parent: ExpressionNode) : ExpressionNode} callback
     * @returns {SymbolNode} Returns a clone of the node
     */
    map(callback: (child: ExpressionNode, path: string, parent: ExpressionNode) => ExpressionNode): SymbolNode {
        return this.clone();
    }


    /**
     * Throws an error 'Undefined symbol {name}'
     * @param {string} name
     */
    public static onUndefinedSymbol(name) {
        throw new Error('Undefined symbol ' + name)
    }

    /**
     * Create a clone of this node, a shallow copy
     * @return {SymbolNode}
     */
    clone(): SymbolNode {
        return new SymbolNode(this.name)
    }

    /**
     * Get string representation
     * @param {Object} options
     * @return {string} str
     * @override
     */
    _toString(options: Object) {
        return this.name
    }


    /**
     * Get a JSON representation of the node
     * @returns {Object}
     */
    toJSON() {
        return {
            mathjs: 'SymbolNode',
            name: this.name
        }
    }

    /**
     * Instantiate a SymbolNode from its JSON representation
     * @param {Object} json  An object structured like
     *                       `{"mathjs": "SymbolNode", name: "x"}`,
     *                       where mathjs is optional
     * @returns {SymbolNode}
     */
    public static fromJSON(json) {
        return new SymbolNode(json.name)
    }




}