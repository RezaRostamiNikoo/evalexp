import { ExpressionNode } from "./ExpressionNode";

export class SymbolNode extends ExpressionNode {
    isSymbolNode: boolean = true;
    s: string;
    constructor(symbol: string) {
        super();
        this.s = symbol;
    }


    toString(): string {
        return this.s;
    }
}