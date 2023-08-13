
import { Scope } from "./Scope";
import { Expression } from "./Expression";
import { Token } from "./Token";
import { TokenType } from "./types";
import { Queue } from "../../utils/Queue";
import { Stack } from "../../utils/Stack";

export class State {
    private scope: Scope;
    private _expression: Expression; // current expression
    private _level: number = 0;
    private _tokens: Queue<Token>;
    private _deletedTokens: Stack<Token> = new Stack();

    constructor(expression: string, scope?: Map<string, any>) {
        this._expression = new Expression(expression);
        this.scope = new Scope(scope, (item: any) => item, (item: any) => item);
    }

    get tokens(): Array<string> { return this._tokens.map(t => t.Value); }

    /**
     * it calculate all the tokens in an expression.
     * @returns {Queue<Token>} return a queue of tokens generated from the expression
     */
    private getTokens(): Queue<Token> {
        if (this._tokens) return this._tokens;
        this._tokens = new Queue();
        while (true) {
            const token = this._expression.getNextToken();
            if (token.Value) this._tokens.enqueue(token);
            else break;
        }
        return this._tokens;
    }

    /**
     * It increases the level of the current and all tokens after that. It means that it set a new level for current token
     * be carefule about head and scouting
    */
    nextLevel() {
        ++this._level;
        this.getTokens().forEach(t => t.Level = this._level);
    }

    /**
     * Close parameters.
     * New line characters will no longer be ignored
     * be carefule about head and scouting
     */
    prevLevel() {
        --this._level;
        this.getTokens().forEach(t => t.Level = this._level);
    }


    /**
     * it return the first token available in the queue without shifring it from the list
     * @returns {Token} returns the current token
     */
    get token(): Token { return this.getTokens().peek() || new Token("", "NULL", this._level); }

    /**
     * It shifts the current token from the queue and next toke is ready to be proccessed on
     * @returns {State} return the main state
     */
    goAHead(): State { this._deletedTokens.push(this.getTokens().dequeue()); return this; }
    rewind(): State { this.getTokens().enqueueAtFirst(this._deletedTokens.pop()); return this }

    /**
     * It checks if current token is equal to the given chars or not.
     * @param {string} chars the character to be compared with current token
     * @returns {boolean} return True if it is equal
     */
    isToken(chars: string): boolean { return this.token.Value === chars; }

    /**
     * It checks if current token type is equal to the given type or not.
     * @param {TokenType} type the character to be compared with current token type
     * @returns {boolean} return True if it is equal
     */
    isType(type: TokenType): boolean { return this.token.Type === type; }


    getErrorOnHead(): string {
        return this._expression.getErrorOnHead(this.token);
    }
}

