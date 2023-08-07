import { IndexNode } from "./IndexNode";
import { ExpressionNode } from "./ExpressionNode";
import { isIndexNode, isNode } from "../../utils/is";

export class AccessorNode extends ExpressionNode {
    isAccessorNode: boolean = true;
    object: ExpressionNode; // TODO: it should be an object if it can be defined in a expression but it can be a symbolNode because it shoud be defined in scope
    index: IndexNode;

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
}