/**
 * Checks whether the current character `c` is a valid alpha character:
 *
 * - A latin letter (upper or lower case) Ascii: a-z, A-Z
 * - An underscore                        Ascii: _
 * - A dollar sign                        Ascii: $
 * - A latin letter with accents          Unicode: \u00C0 - \u02AF
 * - A greek letter                       Unicode: \u0370 - \u03FF
 * - A mathematical alphanumeric symbol   Unicode: \u{1D400} - \u{1D7FF} excluding invalid code points
 *
 * The previous and next characters are needed to determine whether
 * this character is part of a unicode surrogate pair.
 *
 * @param {string} c      Current character in the expression
 * @param {string} cPrev  Previous character
 * @param {string} cNext  Next character
 * @return {boolean}
 */
export function isAlpha(c: string, cPrev: string, cNext: string) {
    return isValidLatinOrGreek(c) ||
        isValidMathSymbol(c, cNext) ||
        isValidMathSymbol(cPrev, c)
}

/**
 * Test whether a character is a valid latin, greek, or letter-like character
 * @param {string} c
 * @return {boolean}
 */
export function isValidLatinOrGreek(c: string) {
    return /^[a-zA-Z_$\u00C0-\u02AF\u0370-\u03FF\u2100-\u214F]$/.test(c)
}

/**
 * Test whether two given 16 bit characters form a surrogate pair of a
 * unicode math symbol.
 *
 * https://unicode-table.com/en/
 * https://www.wikiwand.com/en/Mathematical_operators_and_symbols_in_Unicode
 *
 * Note: In ES6 will be unicode aware:
 * https://stackoverflow.com/questions/280712/javascript-unicode-regexes
 * https://mathiasbynens.be/notes/es6-unicode-regex
 *
 * @param {string} high
 * @param {string} low
 * @return {boolean}
 */
export function isValidMathSymbol(high: string, low: string): boolean {
    return /^[\uD835]$/.test(high) &&
        /^[\uDC00-\uDFFF]$/.test(low) &&
        /^[^\uDC55\uDC9D\uDCA0\uDCA1\uDCA3\uDCA4\uDCA7\uDCA8\uDCAD\uDCBA\uDCBC\uDCC4\uDD06\uDD0B\uDD0C\uDD15\uDD1D\uDD3A\uDD3F\uDD45\uDD47-\uDD49\uDD51\uDEA6\uDEA7\uDFCC\uDFCD]$/.test(low)
}

/**
 * Check whether given character c is a white space character: space, tab, or enter
 * @param {string} c
 * @return {boolean}
 */
export function isWhitespace(c: string): boolean {
    // TODO: also take '\r' carriage return as newline? Or does that give problems on mac?
    return c === ' ' || c === '\t' || c === '\n';;
}

/**
 * Test whether the character c is a decimal mark (dot).
 * This is the case when it's not the start of a delimiter '.*', './', or '.^'
 * @param {string} c
 * @param {string} cNext
 * @return {boolean}
 */
export function isDecimalMark(c: string, cNext: string) {
    return c === '.' && cNext !== '/' && cNext !== '*' && cNext !== '^'
}

/**
 * checks if the given char c is a digit or dot
 * @param {string} c   a string with one character
 * @return {boolean}
 */
export function isDigitDot(c: string) {
    return ((c >= '0' && c <= '9') || c === '.')
}

/**
 * checks if the given char c is a digit
 * @param {string} c   a string with one character
 * @return {boolean}
 */
export function isDigit(c: string) {
    return (c >= '0' && c <= '9')
}

/**
 * checks if the given char c is a hex digit
 * @param {string} c   a string with one character
 * @return {boolean}
 */
export function isHexDigit(c: string) {
    return ((c >= '0' && c <= '9') ||
        (c >= 'a' && c <= 'f') ||
        (c >= 'A' && c <= 'F'))
}