import { ExpressionNode } from "./ExpressionNode";

export class RelationalNode extends ExpressionNode {

    constructor(private firstNode: Array<any>, params: Array<any>) {
        super();

    }
}