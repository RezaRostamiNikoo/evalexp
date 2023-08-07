import { ExpressionNode } from "./Node";

export class RelationalNode extends ExpressionNode {

    constructor(private firstNode: Array<any>, params: Array<any>) {
        super();

    }
}