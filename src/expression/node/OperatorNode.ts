import { ExpressionNode } from "./Node";

export class OperatorNode extends ExpressionNode {

    isPercentage: boolean = false;

    constructor(op: string, fn: string, args: Array<ExpressionNode>, implicit: boolean = false, isPercentage: boolean = false) {
        super();


        this.isPercentage = (isPercentage === true);

    }
}