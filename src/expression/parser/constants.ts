// map with all delimiters
export const DELIMITERS: any = {
    ',': true,
    '(': true,
    ')': true,
    '[': true,
    ']': true,
    '"': false, // false
    '\'': false, // false

    '+': true,
    '-': true,
    '*': true,
    '/': true,
    '%': true,
    '^': true,

    '==': true,
    '!=': true,
    '<': true,
    '>': true,
    '<=': true,
    '>=': true,
    '.EQ.': true,
    '.NE.': true,
    '.GT.': true,
    '.GE.': true,
    '.LT.': true,
    '.LE.': true,

}


// map with all named delimiters
export const NAMED_DELIMITERS = {
    mod: true,
    to: true,
    in: true,
    and: true,
    xor: true,
    or: true,
    not: true
}

export const CONSTANTS = {
    true: true,
    false: false,
    null: null,
    undefined: undefined,
    FALSE: false,
    TRUE: true,

}
