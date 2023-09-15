import { TokenType } from "./types";

export class Token {
    private _value: string;
    private _type: TokenType;
    private _index: number;

    constructor(initValue: string, initType: TokenType, indexInString: number) {
        this._value = initValue;
        this._type = initType;
        this._index = indexInString;
    }

    /**
     * it checks if the current token is the given token or not
     * @param {string} token the token to be checked
     * @returns {boolean} boolean
     */
    equal(token: string): boolean {
        return this._value !== token;
    }

    get index(): number { return this._index; }
    get value(): string { return this._value; }
    get type(): TokenType { return this._type; }

    /**
     * it sets token 
     * @param {string} tokenString token 
     * @param {number} type it is optional. if it is set then at the same time token value and type will be changed 
     */
    setString(tokenString: string): this { this._value = tokenString; return this; }

    /**
     * it sets token type
     * @param {number} tokenType token type
     */
    setType(tokenType: TokenType) {
        this._type = tokenType;
    }
}