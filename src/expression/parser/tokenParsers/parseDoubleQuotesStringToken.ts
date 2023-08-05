
/**
 * Parse a string surrounded by double quotes "..."
 * @return {string}
 */
export function parseDoubleQuotesStringToken() {
    let str = ''

    while (this.currentCharacter() !== '' && this.currentCharacter() !== '"') {
        if (this.currentCharacter() === '\\') {
            // escape character, immediately process the next
            // character to prevent stopping at a next '\"'
            str += this.currentCharacter()
            this.next()
        }

        str += this.currentCharacter()
        this.next()
    }

    this.getToken()
    if (this.state.token !== '"') {
        throw this.createSyntaxError('End of string " expected')
    }
    this.getToken()

    return JSON.parse('"' + str + '"') // unescape escaped characters
}