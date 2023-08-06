export class EmptyExpressionError extends Error {
    name: string;
    message: string;
    stack?: string;

    constructor() {
        super("The sended expression is empty.");
    }
}