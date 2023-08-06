export class ExpressionSyntaxError extends Error {
    name: string;
    message: string;
    stack?: string;

    constructor(token: string) {
        super(`Syntax error in part "${token}"`);
    }
}