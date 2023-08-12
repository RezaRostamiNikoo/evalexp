export function isNumber(x: any) {
    return typeof x === 'number'
}

export function isBigNumber(x: any) {
    if (
        !x || typeof x !== 'object' ||
        typeof x.constructor !== 'function'
    ) {
        return false
    }

    if (
        x.isBigNumber === true &&
        typeof x.constructor.prototype === 'object' &&
        x.constructor.prototype.isBigNumber === true
    ) {
        return true
    }

    if (
        typeof x.constructor.isDecimal === 'function' &&
        x.constructor.isDecimal(x) === true
    ) {
        return true
    }

    return false
}

export function isComplex(x: any) {
    return (x && typeof x === 'object' && Object.getPrototypeOf(x).isComplex === true) || false
}

export function isFraction(x: any) {
    return (x && typeof x === 'object' && Object.getPrototypeOf(x).isFraction === true) || false
}

export function isUnit(x: any) {
    return (x && x.constructor.prototype.isUnit === true) || false
}

export function isString(x: any) {
    return typeof x === 'string'
}

export const isArray = Array.isArray

export function isMatrix(x: any) {
    return (x && x.constructor.prototype.isMatrix === true) || false
}

/**
 * Test whether a value is a collection: an Array or Matrix
 * @param {*} x
 * @returns {boolean} isCollection
 */
export function isCollection(x: any) {
    return Array.isArray(x) || isMatrix(x)
}

export function isDenseMatrix(x: any) {
    return (x && x.isDenseMatrix && x.constructor.prototype.isMatrix === true) || false
}

export function isSparseMatrix(x: any) {
    return (x && x.isSparseMatrix && x.constructor.prototype.isMatrix === true) || false
}

export function isRange(x: any) {
    return (x && x.constructor.prototype.isRange === true) || false
}

export function isIndex(x: any) {
    return (x && x.constructor.prototype.isIndex === true) || false
}

export function isBoolean(x: any) {
    return typeof x === 'boolean'
}

export function isResultSet(x: any) {
    return (x && x.constructor.prototype.isResultSet === true) || false
}

export function isHelp(x: any) {
    return (x && x.constructor.prototype.isHelp === true) || false
}

export function isFunction(x: any) {
    return typeof x === 'function'
}

export function isDate(x: any) {
    return x instanceof Date
}

export function isRegExp(x: any) {
    return x instanceof RegExp
}

export function isObject(x: any) {
    return !!(x &&
        typeof x === 'object' &&
        x.constructor === Object &&
        !isComplex(x) &&
        !isFraction(x))
}

export function isNull(x: any) {
    return x === null
}

export function isUndefined(x: any) {
    return x === undefined
}

export function isAccessorNode(x: any) {
    return (x && x.isAccessorNode === true) || false
}

export function isArrayNode(x: any) {
    return (x && x.isArrayNode === true) || false
}

export function isAssignmentNode(x: any) {
    return (x && x.isAssignmentNode === true) || false
}

export function isBlockNode(x: any) {
    return (x && x.isBlockNode === true) || false
}

export function isConditionalNode(x: any) {
    return (x && x.isConditionalNode === true) || false
}

export function isConstantNode(x: any) {
    return (x && x.isConstantNode === true) || false
}

/* Very specialized: returns true for those nodes which in the numerator of
   a fraction means that the division in that fraction has precedence over implicit
   multiplication, e.g. -2/3 x parses as (-2/3) x and 3/4 x parses as (3/4) x but
   6!/8 x parses as 6! / (8x). It is located here because it is shared between
   parse.js and OperatorNode.js (for parsing and printing, respectively).
 
   This should *not* be exported from mathjs, unlike most of the tests here.
   Its name does not start with 'is' to prevent utils/snapshot.js from thinking
   it should be exported.
*/
export function rule2Node(node: any) {
    return isConstantNode(node) ||
        (isOperatorNode(node) &&
            node.args.length === 1 &&
            isConstantNode(node.args[0]) &&
            '-+'.includes(node.op))
}

export function isFunctionAssignmentNode(x: any) {
    return (x && x.isFunctionAssignmentNode === true) || false
}

export function isFunctionNode(x: any) {
    return (x && x.isFunctionNode === true) || false
}

export function isIndexNode(x: any) {
    return (x && x.isIndexNode === true) || false
}

export function isNode(x: any) {
    return (x && x.isNode === true) || false
}

export function isObjectNode(x: any) {
    return (x && x.isObjectNode === true) || false
}

export function isOperatorNode(x: any) {
    return (x && x.isOperatorNode === true) || false
}

export function isParenthesisNode(x: any) {
    return (x && x.isParenthesisNode === true) || false
}

export function isRangeNode(x: any) {
    return (x && x.isRangeNode === true) || false
}

export function isRelationalNode(x: any) {
    return (x && x.isRelationalNode === true) || false
}

export function isSymbolNode(x: any) {
    return (x && x.isSymbolNode === true) || false
}

export function isChain(x: any) {
    return (x && x.constructor.prototype.isChain === true) || false
}

export function typeOf(x: any) {
    const t = typeof x

    if (t === 'object') {
        if (x === null) return 'null'
        if (isBigNumber(x)) return 'BigNumber' // Special: weird mashup with Decimal
        if (x.constructor && x.constructor.name) return x.constructor.name

        return 'Object' // just in case
    }

    return t // can be 'string', 'number', 'boolean', 'function', 'bigint', ...
}
