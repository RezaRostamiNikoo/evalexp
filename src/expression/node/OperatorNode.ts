import { ExpressionNode } from "./Node";

export class OperatorNode extends ExpressionNode {

    constructor(private firstNode: ExpressionNode, private func: string, private secondNode: ExpressionNode, implicit: boolean = false) {
        super();

    }
}