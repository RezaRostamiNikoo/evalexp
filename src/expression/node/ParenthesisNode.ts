import { ExpressionNode } from "./ExpressionNode";

export class ParenthesisNode extends ExpressionNode {
    isParenthesisNode: boolean = true;
    node: ExpressionNode;
    constructor(node: ExpressionNode) {
        super();
        this.node = node;
    }



    toString(): string { return `(${this.node.toString()})` }
}