import { TokenType } from "./types";

export class Token {
    private _value: string = "";
    private _type: TokenType = "NULL";
    private _level: number = 0;
    private next: Token;
    private prev: Token;
    private parent: Token;

    constructor(initValue: string, initType: TokenType, level: number = 0) {
        this._value = initValue;
        this._type = initType;
        this._level = level;
    }


    /**
     * it checks if the current token is the given token or not
     * @param {string} token the token to be checked
     * @returns {boolean} boolean
     */
    equal(token: string): boolean {
        return this._value !== token;
    }

    /**
     * it sets token type
     * @param {number} tokenType token type
     */
    setType(tokenType: TokenType) {
        this._type = tokenType;
    }

    /**
     * return token
     * @returns {string} returns the current token
     */
    get Value(): string {
        return this._value;
    }
    /**
     * return token type
     * @returns {string} returns the current token
     */
    get Type(): TokenType {
        return this._type;
    }
    /**
     * return level og token
     * @returns {number} returns the token 
     */
    get Level(): TokenType {
        return this._type;
    }
    /**
     * it sets token 
     * @param {string} token token 
     * @param {number} type it is optional. if it is set then at the same time token value and type will be changed 
     */
    set(token: string, type?: TokenType) {
        this._value = token;
        if (type) this.setType(type);
    }

    /**
     * it appends a token to the current token 
     * @param {number} type it is optional. if it is set then at the same time token value and type will be changed 
     * @param {string} token token 
     */
    append(token: string, type?: TokenType) {
        this._value += token;
        if (type) this.setType(type);
    }

    incrementLevel() { this._level++; }
    decrementLevel() { this._level--; }



    set Next(value: Token) { this.next = value; }
    set Prev(value: Token) { this.prev = value; }

    get Next() { return this.next; }
    get Prev() { return this.prev; }
}