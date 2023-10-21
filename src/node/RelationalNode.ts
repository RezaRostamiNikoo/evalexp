import { IScope } from "../interfaces";
import { Stack } from "predefined-ds";
import { getSafeProperty } from "../utils/customs";
import { ExpressionNode } from "./ExpressionNode";

const operatorMap = {
    equal: '==',
    unequal: '!=',
    smaller: '<',
    larger: '>',
    smallerEq: '<=',
    largerEq: '>='
}
export class RelationalNode extends ExpressionNode {
    name: string = "RelationalNode";
    private conditionals: Array<string>;
    private params: Array<ExpressionNode>;

    get type() { return "RelationalNode" }
    get isRelationalNode() { return true }


    /**
     * A node representing a chained conditional expression, such as 'x > y > z'
     *
     * @param {String[]} conditionals
     *     An array of conditional operators used to compare the parameters
     * @param {Node[]} params
     *     The parameters that will be compared
     *
     * @constructor RelationalNode
     * @extends {ExpressionNode}
     */
    constructor(conditionals: Array<string>, params: Array<ExpressionNode>) {
        super();
        if (!Array.isArray(conditionals)) { throw new TypeError('Parameter conditionals must be an array') }
        if (!Array.isArray(params)) { throw new TypeError('Parameter params must be an array') }
        if (conditionals.length !== params.length - 1) {
            throw new TypeError(
                'Parameter params must contain exactly one more element ' +
                'than parameter conditionals')
        }
        this.conditionals = conditionals
        this.params = params

    }



    /**
     * Compile a node into a JavaScript function.
     * This basically pre-calculates as much as possible and only leaves open
     * calculations which depend on a dynamic scope with variables.
     * @param {Object} math     Math.js namespace with functions and constants.
     * @return {(scope: IScope): any} Returns a function which can be called like: evalNode(scope: Object)

     */
    _compile(mathFunctions: Object): (scope: IScope) => any {
        const self = this

        const compiled = this.params.map(p => p._compile(mathFunctions))

        return (scope: IScope) => {
            let evalLhs
            let evalRhs = compiled[0](scope)

            for (let i = 0; i < self.conditionals.length; i++) {
                evalLhs = evalRhs
                evalRhs = compiled[i + 1](scope)
                const condFn = getSafeProperty(mathFunctions, self.conditionals[i])
                if (!condFn(evalLhs, evalRhs)) {
                    return false
                }
            }
            return true
        }
    }

    /**
     * Execute a callback for each of the child nodes of this node
     * @param {function(child: Node, path: string, parent: Node)} callback
     */
    forEach(callback) {
        this.params.forEach((n, i) => callback(n, 'params[' + i + ']', this), this)
    }

    /**
     * Create a new RelationalNode whose children are the results of calling
     * the provided callback function for each child of the original node.
     * @param {function(child: Node, path: string, parent: Node): Node} callback
     * @returns {RelationalNode} Returns a transformed copy of the node
     */
    map(callback) {
        return new RelationalNode(
            this.conditionals.slice(),
            this.params.map(
                (n, i) => this._ifNode(callback(n, 'params[' + i + ']', this)), this))
    }

    /**
     * Create a clone of this node, a shallow copy
     * @return {RelationalNode}
     */
    clone() {
        return new RelationalNode(this.conditionals, this.params)
    }

    /**
     * Get string representation.
     * @param {Object} options
     * @return {string} str
     */
    _toString(options) {
        const p = new Stack<ExpressionNode>(...this.params);
        const c = new Stack<string>(...this.conditionals);
        let result = `${p.pop().evaluate()}`;
        while (c.peek()) {
            result += ` ${c.pop()} `;
            result += `${p.pop()}`;
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
            conditionals: this.conditionals,
            params: this.params
        }
    }

    /**
     * Instantiate a RelationalNode from its JSON representation
     * @param {Object} json
     *     An object structured like
     *     `{"mathjs": "RelationalNode", "conditionals": ..., "params": ...}`,
     *     where mathjs is optional
     * @returns {RelationalNode}
     */
    static fromJSON(json) {
        return new RelationalNode(json.conditionals, json.params)
    }
}