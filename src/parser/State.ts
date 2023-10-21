
import { Tokenizer } from "./Tokenizer";
import { Token } from "./Token";
import { TokenType } from "./types";
import { Queue, Stack } from "predefined-ds";

export class State {
    private _tokenizer: Tokenizer; // current expression
    private _tokens: Queue<Token>;
    private _deletedTokens: Stack<Token> = new Stack();

    constructor(expression: string, scope?: Map<string, any>) {
        this._tokenizer = new Tokenizer(expression);
    }

    get tokens(): Array<string> { return this._tokens.map(t => t.value); }

    /**
     * it calculate all the tokens in an expression.
     * @returns {Queue<Token>} return a queue of tokens generated from the expression
     */
    private getTokens(): Queue<Token> {
        if (this._tokens) return this._tokens;
        this._tokens = new Queue();
        while (true) {
            const token = this._tokenizer.getNextToken();
            if (token) this._tokens.enqueue(token);
            else break;
        }
        return this._tokens;
    }

    /**
     * it return the first token available in the queue without shifring it from the list
     * @returns {Token} returns the current token
     */
    get token(): Token { return this.getTokens().peek(); }

    /**
     * It shifts the current token from the queue and next toke is ready to be proccessed on
     * @returns {State} return the main state
     */
    goAhead(): State { this._deletedTokens.push(this.getTokens().dequeue()); return this; }
    rewind(): State { this.getTokens().enqueueAtFirst(this._deletedTokens.pop()); return this }

    /**
     * It checks if current token is equal to the given chars or not.
     * @param {string} chars the character to be compared with current token
     * @returns {boolean} return True if it is equal
     */
    isToken(chars: string): boolean { return this.token.value === chars; }

    /**
     * It checks if current token type is equal to the given type or not.
     * @param {TokenType} type the character to be compared with current token type
     * @returns {boolean} return True if it is equal
     */
    isType(type: TokenType): boolean { return this.token.type === type; }


    getErrorOnHead(): string {
        return this._tokenizer.getErrorOnHead(this.token);
    }
}

