import { ExpressionRuntimeError, ExpressionSyntaxError } from "../../errors";
import { ConstantNode } from "../../node/ConstantNode";
import { ExpressionNode } from "../../node/Node";
import { OperatorNode } from "../../node/OperatorNode";
import { State } from "../State"
import { parseSymbol } from "./parseSymbol";

/**
 * parse a number
 * @return {Node} node
 * @private
 */
export function parseNumber(state: State): ExpressionNode {
    const token = state.Tokens.peek();

    if (token.Type !== "NUMBER") throw new ExpressionRuntimeError(token.Value);

    if (!token.Next) {
        state.Tokens.dequeue();
        return new ConstantNode(token.Value);
    }

    if (token.Next.Type === "DELIMITER") return parseNumberDelimiter(state);
    if (token.Next.Type === "SYMBOL") return parseNumberSymbol(state);
    if (token.Next.Type === "NUMBER") throw new ExpressionSyntaxError(token.Value);
}


function parseNumberDelimiter(state: State): ExpressionNode {
    const token = state.Tokens.dequeue();
    if (!token.Next) return false;
    if (!(token.Type === "NUMBER" && token.Next.Type === "DELIMITER")) return false;

    const delimiter = token.Next.Value;
    switch (delimiter) {
        case ",":
            return new ConstantNode(token.Value);
        case '(':
            return new OperatorNode(new ConstantNode(token.Value), "multiply", parse_parantheses(this.state));
        case ')':
            return new ConstantNode(token.Value);
        case ']':
            return new ConstantNode(token.Value);

        case '*':
            return new OperatorNode(new ConstantNode(token.Value), "", parse_parantheses(this.state));
        case '/':
            return new OperatorNode(new ConstantNode(token.Value), "", parse_parantheses(this.state));
        case '^':
            return new OperatorNode(new ConstantNode(token.Value), "", parse_parantheses(this.state));
        case '.':
            throw new ExpressionSyntaxError(delimiter);
        case '[':
            throw new ExpressionSyntaxError(delimiter);

    }
    return true;
}


function parseNumberSymbol(state: State): ExpressionNode {
    const token = state.Tokens.dequeue();
    // multiply

    return new OperatorNode(new ConstantNode(token.Value), "", parseSymbol(state));
}