import { State } from "..";

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


export class DigitExpectedException extends Error {

    constructor(pointerOnString: string) {
        super('Digit expected, got \n' + pointerOnString);
    }
}

export class UnknownTokenException extends Error {
    constructor(pointerOnString: string) {
        super('Unknown Token \n' + pointerOnString);
    }
}

export class EmptyExpressionError extends Error {
    name: string;
    message: string;
    stack?: string;

    constructor() {
        super("The sended expression is empty.");
    }
}

export class ExpressionRuntimeError extends Error {
    name: string;
    message: string;
    stack?: string;

    constructor(token: string) {
        super(`Runtime error in part "${token}"`);
    }
}

export class ExpressionSyntaxError extends Error {
    name: string;
    message: string;
    stack?: string;

    constructor(token: string) {
        super(`Syntax error in part "${token}"`);
    }
}