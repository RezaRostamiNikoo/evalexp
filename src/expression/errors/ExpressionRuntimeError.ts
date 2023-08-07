export class ExpressionRuntimeError extends Error {
    name: string;
    message: string;
    stack?: string;

    constructor(token: string) {
        super(`Runtime error in part "${token}"`);
    }
}