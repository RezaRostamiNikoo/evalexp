import { State } from "../parser";

export * from "./EmptyExpressionError";
export * from "./ExpressionSyntaxError";
export * from "./ExpressionRuntimeError";


/**
 * Create an error
 * @param {string} message
 * @return {SyntaxError} instantiated error
 * @private
 */
export function createSyntaxError(state: State, message: string) {
    // const c = this.col();
    // const error = new SyntaxError(message + ' (char ' + c + ')');
    // (error as any).char = c;
    const error = new SyntaxError(message + '\n' + state.getErrorOnHead() + '\n)');

    return error;
}
