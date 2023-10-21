import { IScope } from "../interfaces";
import { isNode } from "../utils/is";
import { ExpressionNode } from "./ExpressionNode";

export class ParenthesesNode extends ExpressionNode {
    name = "ParenthesesNode";
    content: ExpressionNode;

    get type() { return this.name }
    get isParenthesesNode(): boolean { return true }

    /**
     * @constructor ParenthesesNode
     * @extends {ExpressionNode}
     * A parenthesis node describes manual parenthesis from the user input
     * @param {ExpressionNode} content
     * @extends {ExpressionNode}
     */
    constructor(content: ExpressionNode) {
        super();
        // validate input
        if (!isNode(content))
            throw new TypeError('Node expected for parameter "content"')

        this.content = content
    }

    /**
     * Compile a node into a JavaScript function.
     * This basically pre-calculates as much as possible and only leaves open
     * calculations which depend on a dynamic scope with variables.
     * @param {Object} math     Math.js namespace with functions and constants.
     * @return {(scope: IScope): any} Returns a function which can be called like: evalNode(scope: Object)
     */
    _compile(mathFunctions: Object): (scope: IScope) => any {
        return this.content._compile(mathFunctions)
    }

    /**
     * Get the content of the current Node.
     * @return {ExpressionNode} content
     * @override
     **/
    getContent(): ExpressionNode {
        return this.content.getContent()
    }

    /**
     * Execute a callback for each of the child nodes of this node
     * @param {function(child: Node, path: string, parent: Node)} callback
     */
    forEach(callback) {
        callback(this.content, 'content', this)
    }

    /**
     * Create a new ParenthesisNode whose child is the result of calling
     * the provided callback function on the child of this node.
     * @param {function(child: Node, path: string, parent: Node) : Node} callback
     * @returns {ParenthesesNode} Returns a clone of the node
     */
    map(callback) {
        const content = callback(this.content, 'content', this)
        return new ParenthesesNode(content)
    }

    /**
     * Create a clone of this node, a shallow copy
     * @return {ParenthesesNode}
     */
    clone() {
        return new ParenthesesNode(this.content)
    }

    /**
     * Get string representation
     * @param {Object} options
     * @return {string} str
     * @override
     */
    _toString(options) {
        if ((!options) ||
            (options && !options.parenthesis) ||
            (options && options.parenthesis === 'keep')) {
            return '(' + this.content.toString(options) + ')'
        }
        return this.content.toString(options)
    }

    /**
     * Get a JSON representation of the node
     * @returns {Object}
     */
    toJSON() {
        return { mathjs: this.name, content: this.content }
    }

    /**
     * Instantiate an ParenthesisNode from its JSON representation
     * @param {Object} json  An object structured like
     *                       `{"mathjs": "ParenthesisNode", "content": ...}`,
     *                       where mathjs is optional
     * @returns {ParenthesesNode}
     */
    static fromJSON(json) {
        return new ParenthesesNode(json.content)
    }


}