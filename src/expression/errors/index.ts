export * from "./EmptyExpressionError";
export * from "./ExpressionSyntaxError";
export * from "./ExpressionRuntimeError";


/**
 * Create an error
 * @param {string} message
 * @return {SyntaxError} instantiated error
 * @private
 */
export function createSyntaxError(message: string) {
    const c = this.col();
    const error = new SyntaxError(message + ' (char ' + c + ')');
    (error as any).char = c;

    return error;
}

/**
 * Create an error
 * @param {string} message
 * @return {Error} instantiated error
 * @private
 */
export function createError(message: string) {
    const c = this.col();
    const error = new SyntaxError(message + ' (char ' + c + ')');
    (error as any).char = c;

    return error;
}