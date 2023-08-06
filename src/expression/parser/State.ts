
import { hasOwnProperty } from "../../utils/object";
import { ExpressionSyntaxError } from "../errors";
import { NAMED_DELIMITERS } from "./constants"
import { Scope } from "./Scope";
import { Expression } from "./Expression";
import { Token } from "./Token";
import { TokenType } from "./types";
import { Stack } from "../../utils/Stack";
import { Queue } from "../../utils/Queue";

export class State {
    private scope: Scope;
    private _expression: Expression; // current expression
    // get currentLevel() { return this.state.currentLevel; }
    // get conditionalLevel() { return this.state.conditionalLevel; }

    private _tokens: Queue<Token>;

    constructor(expression: string, scope?: Map<string, any>) {
        this._expression = new Expression(expression);
        this.scope = new Scope(scope, (item: any) => item, (item: any) => item);
    }

    get Tokens(): Queue<Token> {
        if (this._tokens) return this._tokens;
        this._tokens = new Queue();
        while (true) {
            const token = this._expression.getNextToken();
            if (token) this._tokens.enqueue(token);
            else break;
        }
        return this._tokens;
    }

    /**
     * Open parameters.
     * New line characters will be ignored untilthis.closeParams() is called
     */
    openParams() {
        // this.state.currentLevel++
    }

    /**
     * Close parameters.
     * New line characters will no longer be ignored
     */
    closeParams() {
        // this.state.currentLevel--
    }

}

