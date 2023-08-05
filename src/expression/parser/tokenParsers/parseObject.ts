import { TOKENTYPE } from "../../constants"
import { parseRelational } from "./parseRelational"

/**
 * parse an object, enclosed in angle brackets{...}, for example {value: 2}
 * @return {Node} node
 * @private
 */
export function parseObject() {
    if (this.state.token === '{') {
        this.openParams()
        let key

        const properties: any = {}
        do {
            this.getToken()

            if (!this.isToken('}')) {
                // parse key
                if (this.isToken('"')) {
                    key = this.parseDoubleQuotesStringToken()
                } else if (this.isToken('\'')) {
                    key = this.parseSingleQuotesStringToken()
                } else if (this.state.tokenType === TOKENTYPE.SYMBOL || (this.state.tokenType === TOKENTYPE.DELIMITER && this.state.token in NAMED_DELIMITERS)) {
                    key = this.state.token
                    this.getToken()
                } else {
                    throw this.createSyntaxError('Symbol or string expected as object key')
                }

                // parse key/value separator
                if (!this.isToken(':')) {
                    throw this.createSyntaxError('Colon : expected after object key')
                }
                this.getToken()

                // parse key
                properties[key] = parseRelational(state)
            }
        }
        while (this.isToken(',')) // eslint-disable-line no-unmodified-loop-condition

        if (!this.isToken('}')) {
            throw this.createSyntaxError('Comma , or bracket } expected after object value')
        }
        this.closeParams()
        this.getToken()

        let node = new ObjectNode(properties)

        // parse index parameters
        node = this.parseAccessors(node)

        return node
    }

    return this.parseNumber()
}