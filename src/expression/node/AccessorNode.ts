import { IndexNode } from "./IndexNode";
import { ExpressionNode } from "./Node";

export class AccessorNode {
    isAccessorNode: boolean = true;

    constructor(node: ExpressionNode, indexNode: IndexNode) {

    }
}