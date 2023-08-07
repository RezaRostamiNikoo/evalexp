import { isNode } from "../../utils/is";
import { ExpressionNode } from "./ExpressionNode";

export class OperatorNode extends ExpressionNode {

    isPercentage: boolean = false;
    implicit: boolean = false;
    op: string;
    fn: string;
    args: Array<ExpressionNode>;
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

}