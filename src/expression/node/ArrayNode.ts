import { ExpressionNode } from "./ExpressionNode";

export class ArrayNode extends ExpressionNode {
    isArrayNode: boolean = true;
    args: Array<ExpressionNode>;

    constructor(items: Array<any>) {
        super();
        this.args = items;
    }


    toString(): string {
        return `[${this.args.map(a => a.toString()).join(", ")}]`
    }
}