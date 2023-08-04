import { TOKENTYPE } from "../constants";

export class Token {
    private _value: string = '';
    private _type: number = TOKENTYPE.NULL;

    constructor(initValue: string, initType: number) {
        this._value = initValue;
        this._type = initType;
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
    setType(tokenType: number) {
        this._type = tokenType;
    }

    /**
     * return token
     * @returns {string} returns the current token
     */
    getValue(): string {
        return this._value;
    }
    /**
     * return token type
     * @returns {string} returns the current token
     */
    getType(): number {
        return this._type;
    }
    /**
     * it sets token 
     * @param {string} token token 
     * @param {number} type it is optional. if it is set then at the same time token value and type will be changed 
     */
    set(token: string, type?: number) {
        this._value = token;
        if (type) this.setType(type);
    }

    /**
     * it appends a token to the current token 
     * @param {number} type it is optional. if it is set then at the same time token value and type will be changed 
     * @param {string} token token 
     */
    append(token: string, type?: number) {
        this._value += token;
        if (type) this.setType(type);
    }


    setTypeTo_DELIMITER() {
        this.setType(TOKENTYPE.DELIMITER);
    }
    setTypeTo_Null() {
        this.setType(TOKENTYPE.NULL);
    }
    setTypeTo_UNKNOWN() {
        this.setType(TOKENTYPE.UNKNOWN);
    }
    setTypeTo_NUMBER() {
        this.setType(TOKENTYPE.NUMBER);
    }
}