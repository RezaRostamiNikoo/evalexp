// /**
//  * this is from mathjs library
//  * date of using: 02 Aug 2023
//  * github link: https://github.com/josdejong/mathjs
//  * version: npm v11.9.1 
//  * commit: https://github.com/josdejong/mathjs/commit/59320053fd35e64351713c4ef32af37df1f4c425
//  */

// import { isAccessorNode, isConstantNode, isFunctionNode, isOperatorNode, isSymbolNode } from "../../utils/is";
// import { hasOwnProperty } from "../../utils/object";
// import { CONSTANTS, DELIMITERS, NAMED_DELIMITERS, NUMERIC_CONSTANTS, TOKENTYPE } from "../constants"
// import { State } from "./State";

// import * as helper from "./helper";

// export class Parser {
//     private state: State;






//     /**
//      * Start of the parse levels below, in order of precedence
//      * @return {Node} node
//      * @private
//      */
//     parseStart(expression: string, extraNodes) {
//         const state = new State(expression, extraNodes);
//         state.getToken()

//         const node = this.parseBlock()

//         // check for garbage at the end of the expression
//         // an expression ends with a empty character '' and tokenType DELIMITER
//         if (this.state.token !== '') {
//             if (this.state.tokenType === TOKENTYPE.DELIMITER) {
//                 // user entered a not existing operator like "//"

//                 // TODO: give hints for aliases, for example with "<>" give as hint " did you mean !== ?"
//                 throw this.createError('Unexpected operator ' + this.state.token)
//             } else {
//                 throw this.createSyntaxError('Unexpected part "' + this.state.token + '"')
//             }
//         }

//         return node
//     }

//     /**
//      * Parse a block with expressions. Expressions can be separated by a newline
//      * character '\n', or by a semicolon ';'. In case of a semicolon, no output
//      * of the preceding line is returned.
//      * @return {Node} node
//      * @private
//      */
//     parseBlock() {
//         let node;
//         const blocks = [];
//         let visible;

//         if (this.state.token !== '' && this.state.token !== '\n' && this.state.token !== ';') {
//             node = this.parseAssignment()
//             if (this.state.comment) {
//                 node.comment = this.state.comment
//             }
//         }

//         // TODO: simplify this loop
//         while (this.isToken('\n') || this.isToken(';')) { // eslint-disable-line no-unmodified-loop-condition
//             if (blocks.length === 0 && node) {
//                 visible = (!this.isToken(';'))
//                 blocks.push({ node, visible })
//             }

//             this.getToken()
//             if (this.state.token !== '\n' && this.state.token !== ';' && this.state.token !== '') {
//                 node = this.parseAssignment()
//                 if (this.state.comment) {
//                     node.comment = this.state.comment
//                 }

//                 visible = (this.state.token !== ';')
//                 blocks.push({ node, visible })
//             }
//         }

//         if (blocks.length > 0) {
//             return new BlockNode(blocks)
//         } else {
//             if (!node) {
//                 node = new ConstantNode(undefined)
//                 if (this.state.comment) {
//                     node.comment = this.state.comment
//                 }
//             }

//             return node
//         }
//     }

//     /**
//      * Assignment of a function or variable,
//      * - can be a variable like 'a=2.3'
//      * - or a updating an existing variable likethis.'matrix(2,3:5)=[6,7,8]'
//      * - defining a function likethis.'f(x) = x^2'
//      * @return {Node} node
//      * @private
//      */
//     parseAssignment() {
//         let name, args, value, valid

//         const node = this.parseConditional()

//         if (this.isToken('=')) {
//             if (isSymbolNode(node)) {
//                 // parse a variable assignment like 'a = 2/3'
//                 name = node.name
//                 this.getTokenSkipNewline()
//                 value = this.parseAssignment()
//                 return new AssignmentNode(new SymbolNode(name), value)
//             } else if (isAccessorNode(node)) {
//                 // parse a matrix subset assignment like 'A[1,2] = 4'
//                 this.getTokenSkipNewline()
//                 value = this.parseAssignment()
//                 return new AssignmentNode(node.object, node.index, value)
//             } else if (isFunctionNode(node) && isSymbolNode(node.fn)) {
//                 // parse function assignment likethis.'f(x) = x^2'
//                 valid = true
//                 args = []

//                 name = node.name
//                 node.args.forEach(function (arg, index) {
//                     if (isSymbolNode(arg)) {
//                         args[index] = arg.name
//                     } else {
//                         valid = false
//                     }
//                 })

//                 if (valid) {
//                     this.getTokenSkipNewline()
//                     value = this.parseAssignment()
//                     return new FunctionAssignmentNode(name, args, value)
//                 }
//             }

//             throw this.createSyntaxError('Invalid left hand side of assignment operator =')
//         }

//         return node
//     }

//     /**
//      * conditional operation
//      *
//      *     condition ? truePart : falsePart
//      *
//      * Note: conditional operator is right-associative
//      *
//      * @return {Node} node
//      * @private
//      */
//     parseConditional() {
//         let node = this.parseLogicalOr()

//         while (this.state.token === '?') { // eslint-disable-line no-unmodified-loop-condition
//             // set a conditional level, the range operator will be ignored as long
//             // as conditionalLevel === this.state.nestingLevel.
//             const prev = this.state.conditionalLevel
//             this.state.conditionalLevel = this.state.nestingLevel
//             this.getTokenSkipNewline()

//             const condition = node
//             const trueExpr = this.parseAssignment()

//             if (this.state.token !== ':') throw createSyntaxError('False part of conditional expression expected')

//             this.state.conditionalLevel = null
//             this.getTokenSkipNewline()

//             const falseExpr = this.parseAssignment() // Note: check for conditional operator again, right associativity

//             node = new ConditionalNode(condition, trueExpr, falseExpr)

//             // restore the previous conditional level
//             this.state.conditionalLevel = prev
//         }

//         return node
//     }

//     /**
//      * logical or, 'x or y'
//      * @return {Node} node
//      * @private
//      */
//     parseLogicalOr() {
//         let node = this.parseLogicalXor()

//         while (this.state.token === 'or') { // eslint-disable-line no-unmodified-loop-condition
//             this.getTokenSkipNewline()
//             node = new OperatorNode('or', 'or', [node, this.parseLogicalXor()])
//         }

//         return node
//     }

//     /**
//      * logical exclusive or, 'x xor y'
//      * @return {Node} node
//      * @private
//      */
//     parseLogicalXor() {
//         let node = this.parseLogicalAnd()

//         while (this.state.token === 'xor') { // eslint-disable-line no-unmodified-loop-condition
//             this.getTokenSkipNewline()
//             node = new OperatorNode('xor', 'xor', [node, this.parseLogicalAnd()])
//         }

//         return node
//     }

//     /**
//      * logical and, 'x and y'
//      * @return {Node} node
//      * @private
//      */
//     parseLogicalAnd() {
//         let node = this.parseBitwiseOr()

//         while (this.state.token === 'and') { // eslint-disable-line no-unmodified-loop-condition
//             this.getTokenSkipNewline()
//             node = new OperatorNode('and', 'and', [node, this.parseBitwiseOr()])
//         }

//         return node
//     }

//     /**
//      * bitwise or, 'x | y'
//      * @return {Node} node
//      * @private
//      */
//     parseBitwiseOr() {
//         let node = this.parseBitwiseXor()

//         while (this.state.token === '|') { // eslint-disable-line no-unmodified-loop-condition
//             this.getTokenSkipNewline()
//             node = new OperatorNode('|', 'bitOr', [node, this.parseBitwiseXor()])
//         }

//         return node
//     }

//     /**
//      * bitwise exclusive or (xor), 'x ^| y'
//      * @return {Node} node
//      * @private
//      */
//     parseBitwiseXor() {
//         let node = this.parseBitwiseAnd()

//         while (this.state.token === '^|') { // eslint-disable-line no-unmodified-loop-condition
//             this.getTokenSkipNewline()
//             node = new OperatorNode('^|', 'bitXor', [node, this.parseBitwiseAnd()])
//         }

//         return node
//     }

//     /**
//      * bitwise and, 'x & y'
//      * @return {Node} node
//      * @private
//      */
//     parseBitwiseAnd() {
//         let node = this.parseRelational()

//         while (this.state.token === '&') { // eslint-disable-line no-unmodified-loop-condition
//             this.getTokenSkipNewline()
//             node = new OperatorNode('&', 'bitAnd', [node, this.parseRelational()])
//         }

//         return node
//     }

//     /**
//      * Parse a chained conditional, like 'a > b >= c'
//      * @return {Node} node
//      */
//     parseRelational() {
//         const params = this.[parseShift()]
//         const conditionals = []

//         const operators = {
//             '==': 'equal',
//             '!=': 'unequal',
//             '<': 'smaller',
//             '>': 'larger',
//             '<=': 'smallerEq',
//             '>=': 'largerEq'
//         }

//         while (hasOwnProperty(operators, this.state.token)) { // eslint-disable-line no-unmodified-loop-condition
//             const cond = { name: this.state.token, fn: operators[this.state.token] }
//             conditionals.push(cond)
//             this.getTokenSkipNewline()
//             params.push(parseShift())
//         }

//         if (params.length === 1) {
//             return params[0]
//         } else if (params.length === 2) {
//             return new OperatorNode(conditionals[0].name, conditionals[0].fn, params)
//         } else {
//             return new RelationalNode(conditionals.map(c => c.fn), params)
//         }
//     }

//     /**
//      * Bitwise left shift, bitwise right arithmetic shift, bitwise right logical shift
//      * @return {Node} node
//      * @private
//      */
//     parseShift() {
//         let node, name, fn, params

//         node = this.parseConversion()

//         const operators = {
//             '<<': 'leftShift',
//             '>>': 'rightArithShift',
//             '>>>': 'rightLogShift'
//         }

//         while (hasOwnProperty(operators, this.state.token)) {
//             name = this.state.token
//             fn = operators[name]

//             this.getTokenSkipNewline()
//             params = [node, this.parseConversion()]
//             node = new OperatorNode(name, fn, params)
//         }

//         return node
//     }

//     /**
//      * conversion operators 'to' and 'in'
//      * @return {Node} node
//      * @private
//      */
//     parseConversion() {
//         let node, name, fn, params

//         node = this.parseRange()

//         const operators = {
//             to: 'to',
//             in: 'to' // alias of 'to'
//         }

//         while (hasOwnProperty(operators, this.state.token)) {
//             name = this.state.token
//             fn = operators[name]

//             this.getTokenSkipNewline()

//             if (name === 'in' && this.state.token === '') {
//                 // end of expression -> this is the unit 'in' ('inch')
//                 node = new OperatorNode('*', 'multiply', [node, new SymbolNode('in')], true)
//             } else {
//                 // operator 'a to b' or 'a in b'
//                 params = [node, this.parseRange()]
//                 node = new OperatorNode(name, fn, params)
//             }
//         }

//         return node
//     }

//     /**
//      * parse range, "start:end", "start:step:end", ":", "start:", ":end", etc
//      * @return {Node} node
//      * @private
//      */
//     parseRange() {
//         let node
//         const params = []

//         if (this.state.token === ':') {
//             // implicit start=1 (one-based)
//             node = new ConstantNode(1)
//         } else {
//             // explicit start
//             node = this.parseAddSubtract()
//         }

//         if (this.state.token === ':' && (this.state.conditionalLevel !== this.state.nestingLevel)) {
//             // we ignore the range operator when a conditional operator is being processed on the same level
//             params.push(node)

//             // parse step and end
//             while (this.state.token === ':' && params.length < 3) { // eslint-disable-line no-unmodified-loop-condition
//                 this.getTokenSkipNewline()

//                 if (this.state.token === ')' || this.state.token === ']' || this.state.token === ',' || this.state.token === '') {
//                     // implicit end
//                     params.push(new SymbolNode('end'))
//                 } else {
//                     // explicit end
//                     params.push(parseAddSubtract())
//                 }
//             }

//             if (params.length === 3) {
//                 // params = [start, step, end]
//                 node = new RangeNode(params[0], params[2], params[1]) // start, end, step
//             } else { // length === 2
//                 // params = [start, end]
//                 node = new RangeNode(params[0], params[1]) // start, end
//             }
//         }

//         return node
//     }

//     /**
//      * add or subtract
//      * @return {Node} node
//      * @private
//      */
//     parseAddSubtract() {
//         let node, name, fn, params

//         node = this.parseMultiplyDivide()

//         const operators = {
//             '+': 'add',
//             '-': 'subtract'
//         }
//         while (hasOwnProperty(operators, this.state.token)) {
//             name = this.state.token
//             fn = operators[name]

//             this.getTokenSkipNewline()
//             const rightNode = this.parseMultiplyDivide()
//             if (rightNode.isPercentage) {
//                 params = [node, new OperatorNode('*', 'multiply', [node, rightNode])]
//             } else {
//                 params = [node, rightNode]
//             }
//             node = new OperatorNode(name, fn, params)
//         }

//         return node
//     }

//     /**
//      * multiply, divide, modulus
//      * @return {Node} node
//      * @private
//      */
//     parseMultiplyDivide() {
//         let node, last, name, fn

//         node = this.parseImplicitMultiplication()
//         last = node

//         const operators = {
//             '*': 'multiply',
//             '.*': 'dotMultiply',
//             '/': 'divide',
//             './': 'dotDivide'
//         }

//         while (true) {
//             if (hasOwnProperty(operators, this.state.token)) {
//                 // explicit operators
//                 name = this.state.token
//                 fn = operators[name]

//                 this.getTokenSkipNewline()

//                 last = this.parseImplicitMultiplication()
//                 node = new OperatorNode(name, fn, [node, last])
//             } else {
//                 break
//             }
//         }

//         return node
//     }

//     /**
//      * implicit multiplication
//      * @return {Node} node
//      * @private
//      */
//     parseImplicitMultiplication() {
//         let node, last

//         node = this.parseRule2()
//         last = node

//         while (true) {
//             if ((this.state.tokenType === TOKENTYPE.SYMBOL) ||
//                 (this.state.token === 'in' && isConstantNode(node)) ||
//                 (this.state.tokenType === TOKENTYPE.NUMBER &&
//                     !isConstantNode(last) &&
//                     (!isOperatorNode(last) || last.op === '!')) ||
//                 (this.state.token === '(')) {
//                 // parse implicit multiplication
//                 //
//                 // symbol:      implicit multiplication like '2a', '(2+3)a', 'a b'
//                 // number:      implicit multiplication like '(2+3)2'
//                 // parenthesis: implicit multiplication likethis.'2(3+4)', '(3+4)(1+2)'
//                 last = this.parseRule2()
//                 node = new OperatorNode('*', 'multiply', [node, last], true /* implicit */)
//             } else {
//                 break
//             }
//         }

//         return node
//     }

//     /**
//      * Infamous "rule 2" as described in https://github.com/josdejong/mathjs/issues/792#issuecomment-361065370
//      * And as amended in https://github.com/josdejong/mathjs/issues/2370#issuecomment-1054052164
//      * Explicit division gets higher precedence than implicit multiplication
//      * when the division matches this pattern:
//      *   [unaryPrefixOp]?[number] / [number] [symbol]
//      * @return {Node} node
//      * @private
//      */
//     parseRule2() {
//         let node = this.parsePercentage()
//         let last = node
//         const tokenStates = []

//         while (true) {
//             // Match the "number /" part of the pattern "number / number symbol"
//             if (this.isToken('/') && this.rule2Node(last)) {
//                 // Look ahead to see if the next token is a number
//                 tokenStates.push(Object.assign({}, state))
//                 this.getTokenSkipNewline()

//                 // Match the "number / number" part of the pattern
//                 if (this.state.tokenType === TOKENTYPE.NUMBER) {
//                     // Look ahead again
//                     tokenStates.push(Object.assign({}, state))
//                     this.getTokenSkipNewline()

//                     // Match the "symbol" part of the pattern, or a left parenthesis
//                     if (this.state.tokenType === TOKENTYPE.SYMBOL || this.state.token === '(') {
//                         // We've matched the pattern "number / number symbol".
//                         // Rewind once and build the "number / number" node; the symbol will be consumed later
//                         Object.assign(state, tokenStates.pop())
//                         tokenStates.pop()
//                         last = this.parsePercentage()
//                         node = new OperatorNode('/', 'divide', [node, last])
//                     } else {
//                         // Not a match, so rewind
//                         tokenStates.pop()
//                         Object.assign(state, tokenStates.pop())
//                         break
//                     }
//                 } else {
//                     // Not a match, so rewind
//                     Object.assign(state, tokenStates.pop())
//                     break
//                 }
//             } else {
//                 break
//             }
//         }

//         return node
//     }

//     /**
//      * percentage or mod
//      * @return {Node} node
//      * @private
//      */
//     parsePercentage() {
//         let node, name, fn, params

//         node = this.parseUnary()

//         const operators = {
//             '%': 'mod',
//             mod: 'mod'
//         }
//         while (hasOwnProperty(operators, this.state.token)) {
//             name = this.state.token
//             fn = operators[name]

//             this.getTokenSkipNewline()

//             if (name === '%' && this.state.tokenType === TOKENTYPE.DELIMITER && this.state.token !== '(') {
//                 // If the expression contains only %, then treat that as /100
//                 node = new OperatorNode('/', 'divide', [node, new ConstantNode(100)], false, true)
//             } else {
//                 params = [node, this.parseUnary()]
//                 node = new OperatorNode(name, fn, params)
//             }
//         }

//         return node
//     }

//     /**
//      * Unary plus and minus, and logical and bitwise not
//      * @return {Node} node
//      * @private
//      */
//     parseUnary() {
//         let name, params, fn
//         const operators = {
//             '-': 'unaryMinus',
//             '+': 'unaryPlus',
//             '~': 'bitNot',
//             not: 'not'
//         }

//         if (hasOwnProperty(operators, this.state.token)) {
//             fn = operators[this.state.token]
//             name = this.state.token

//             this.getTokenSkipNewline()
//             params = this.[parseUnary()]

//             return new OperatorNode(name, fn, params)
//         }

//         return parsePow()
//     }

//     /**
//      * power
//      * Note: power operator is right associative
//      * @return {Node} node
//      * @private
//      */
//     parsePow() {
//         let node, name, fn, params

//         node = this.parseLeftHandOperators()

//         if (this.state.token === '^' || this.state.token === '.^') {
//             name = this.state.token
//             fn = (name === '^') ? 'pow' : 'dotPow'

//             this.getTokenSkipNewline()
//             params = [node, this.parseUnary()] // Go back to unary, we can have '2^-3'
//             node = new OperatorNode(name, fn, params)
//         }

//         return node
//     }

//     /**
//      * Left hand operators: factorial x!, ctranspose x'
//      * @return {Node} node
//      * @private
//      */
//     parseLeftHandOperators() {
//         let node, name, fn, params

//         node = this.parseCustomNodes()

//         const operators = {
//             '!': 'factorial',
//             '\'': 'ctranspose'
//         }

//         while (hasOwnProperty(operators, this.state.token)) {
//             name = this.state.token
//             fn = operators[name]

//             this.getToken()
//             params = [node]

//             node = new OperatorNode(name, fn, params)
//             node = this.parseAccessors(node)
//         }

//         return node
//     }

//     /**
//      * Parse a custom node handler. A node handler can be used to process
//      * nodes in a custom way, for example for handling a plot.
//      *
//      * A handler must be passed as second argument of the parse function.
//      * - must extend math.Node
//      * - must contain a functionthis._compile(defs: Object) : string
//      * - must contain a functionthis.find(filter: Object) : Node[]
//      * - must contain a functionthis.toString() : string
//      * - the constructor is called with a single argument containing all parameters
//      *
//      * For example:
//      *
//      *     nodes = {
//      *       'plot': PlotHandler
//      *     }
//      *
//      * The constructor of the handler is called as:
//      *
//      *     node = new PlotHandler(params)
//      *
//      * The handler will be invoked when evaluating an expression like:
//      *
//      *     node = math.parse('plot(sin(x), x)', nodes)
//      *
//      * @return {Node} node
//      * @private
//      */
//     parseCustomNodes() {
//         let params = []

//         if (this.state.tokenType === TOKENTYPE.SYMBOL && this.hasOwnProperty(this.state.extraNodes, this.state.token)) {
//             const CustomNode = this.state.extraNodes[this.state.token]

//             this.getToken()

//             // parse parameters
//             if (this.state.token === '(') {
//                 params = []

//                 this.openParams()
//                 this.getToken()

//                 if (!this.isToken(')')) {
//                     params.push(this.parseAssignment())

//                     // parse a list with parameters
//                     while (this.isToken(',')) { // eslint-disable-line no-unmodified-loop-condition
//                         this.getToken()
//                         params.push(this.parseAssignment())
//                     }
//                 }

//                 if (!this.isToken(')')) {
//                     throw this.createSyntaxError('Parenthesis ) expected')
//                 }
//                 this.closeParams()
//                 this.getToken()
//             }

//             // create a new custom node
//             // noinspection JSValidateTypes
//             return new CustomNode(params)
//         }

//         return this.parseSymbol()
//     }

//     /**
//      * parse symbols: functions, variables, constants, units
//      * @return {Node} node
//      * @private
//      */
//     parseSymbol() {
//         let node, name

//         if (this.state.tokenType === TOKENTYPE.SYMBOL ||
//             (this.state.tokenType === TOKENTYPE.DELIMITER && this.state.token in NAMED_DELIMITERS)) {
//             name = this.state.token

//             this.getToken()

//             if (hasOwnProperty(CONSTANTS, name)) { // true, false, null, ...
//                 console.log("node = new ConstantNode(CONSTANTS[name])", CONSTANTS, name);
//                 // node = new ConstantNode(CONSTANTS[name])
//             } else if (NUMERIC_CONSTANTS.indexOf(name) !== -1) { // NaN, Infinity
//                 console.log("node = new ConstantNode(numeric(name, 'number'))", NUMERIC_CONSTANTS, name);
//                 // node = new ConstantNode(numeric(name, 'number'))
//             } else {
//                 console.log("node = new SymbolNode(name)", name);
//                 // node = new SymbolNode(name)
//             }

//             // parse function parameters and matrix index
//             node = this.parseAccessors(node)
//             return node
//         }

//         return this.parseDoubleQuotesString()
//     }

//     /**
//      * parse accessors:
//      * - function invocation in round brackets (...), for examplethis.sqrt(2)
//      * - index enclosed in square brackets [...], for example A[2,3]
//      * - dot notation for properties, like foo.bar
//      * @param {Object} state
//      * @param {Node} node    Node on which to apply the parameters. If there
//      *                       are no parameters in the expression, the node
//      *                       itself is returned
//      * @param {string[]} [types]  Filter the types of notations
//      *                            can be ['(', '[', '.']
//      * @return {Node} node
//      * @private
//      */
//     parseAccessors(node, types?) {
//         let params

//         while ((this.state.token === '(' || this.state.token === '[' || this.state.token === '.') &&
//             (!types || types.indexOf(this.state.token) !== -1)) { // eslint-disable-line no-unmodified-loop-condition
//             params = []

//             if (this.isToken('(')) {
//                 if (isSymbolNode(node) || isAccessorNode(node)) {
//                     // function invocation likethis.fn(2, 3) or obj.fn(2, 3)
//                     this.openParams()
//                     this.getToken()

//                     if (!this.isToken(')')) {
//                         params.push(this.parseAssignment())

//                         // parse a list with parameters
//                         while (this.isToken(',')) { // eslint-disable-line no-unmodified-loop-condition
//                             this.getToken()
//                             params.push(this.parseAssignment())
//                         }
//                     }

//                     if (!this.isToken(')')) {
//                         throw this.createSyntaxError('Parenthesis ) expected')
//                     }
//                     this.closeParams()
//                     this.getToken()

//                     node = new FunctionNode(node, params)
//                 } else {
//                     // implicit multiplication like (2+3)(4+5) orthis.sqrt(2)(1+2)
//                     // don't parse it here but let it be handled by parseImplicitMultiplication
//                     // with correct precedence
//                     return node
//                 }
//             } else if (this.state.token === '[') {
//                 // index notation like variable[2, 3]
//                 this.openParams()
//                 this.getToken()

//                 if (!this.isToken(']')) {
//                     params.push(this.parseAssignment())

//                     // parse a list with parameters
//                     while (this.isToken(',')) { // eslint-disable-line no-unmodified-loop-condition
//                         this.getToken()
//                         params.push(this.parseAssignment())
//                     }
//                 }

//                 if (!this.isToken(']')) {
//                     throw this.createSyntaxError('Parenthesis ] expected')
//                 }
//                 this.closeParams()
//                 this.getToken()

//                 node = new AccessorNode(node, new IndexNode(params))
//             } else {
//                 // dot notation like variable.prop
//                 this.getToken()

//                 if (this.state.tokenType !== TOKENTYPE.SYMBOL) {
//                     throw this.createSyntaxError('Property name expected after dot')
//                 }
//                 params.push(new ConstantNode(this.state.token))
//                 this.getToken()

//                 const dotNotation = true
//                 node = new AccessorNode(node, new IndexNode(params, dotNotation))
//             }
//         }

//         return node
//     }

//     /**
//      * Parse a double quotes string.
//      * @return {Node} node
//      * @private
//      */
//     parseDoubleQuotesString() {
//         let node, str

//         if (this.state.token === '"') {
//             str = this.parseDoubleQuotesStringToken()

//             // create constant
//             node = new ConstantNode(str)

//             // parse index parameters
//             node = this.parseAccessors(node)

//             return node
//         }

//         return this.parseSingleQuotesString()
//     }

//     /**
//      * Parse a string surrounded by double quotes "..."
//      * @return {string}
//      */
//     parseDoubleQuotesStringToken() {
//         let str = ''

//         while (this.currentCharacter() !== '' && this.currentCharacter() !== '"') {
//             if (this.currentCharacter() === '\\') {
//                 // escape character, immediately process the next
//                 // character to prevent stopping at a next '\"'
//                 str += this.currentCharacter()
//                 this.next()
//             }

//             str += this.currentCharacter()
//             this.next()
//         }

//         this.getToken()
//         if (this.state.token !== '"') {
//             throw this.createSyntaxError('End of string " expected')
//         }
//         this.getToken()

//         return JSON.parse('"' + str + '"') // unescape escaped characters
//     }

//     /**
//      * Parse a single quotes string.
//      * @return {Node} node
//      * @private
//      */
//     parseSingleQuotesString() {
//         let node, str

//         if (this.state.token === '\'') {
//             str = this.parseSingleQuotesStringToken()

//             // create constant
//             node = new ConstantNode(str)

//             // parse index parameters
//             node = this.parseAccessors(node)

//             return node
//         }

//         return this.parseMatrix()
//     }

//     /**
//      * Parse a string surrounded by single quotes '...'
//      * @return {string}
//      */
//     parseSingleQuotesStringToken() {
//         let str = ''

//         while (this.currentCharacter() !== '' && this.currentCharacter() !== '\'') {
//             if (this.currentCharacter() === '\\') {
//                 // escape character, immediately process the next
//                 // character to prevent stopping at a next '\''
//                 str += this.currentCharacter()
//                 this.next()
//             }

//             str += this.currentCharacter()
//             this.next()
//         }

//         this.getToken()
//         if (this.state.token !== '\'') {
//             throw this.createSyntaxError('End of string \' expected')
//         }
//         this.getToken()

//         return JSON.parse('"' + str + '"') // unescape escaped characters
//     }

//     /**
//      * parse the matrix
//      * @return {Node} node
//      * @private
//      */
//     parseMatrix() {
//         let array, params, rows, cols

//         if (this.state.token === '[') {
//             // matrix [...]
//             this.openParams()
//             this.getToken()

//             if (!this.isToken(']')) {
//                 // this is a non-empty matrix
//                 const row = this.parseRow()

//                 if (this.isToken(';')) {
//                     // 2 dimensional array
//                     rows = 1
//                     params = [row]

//                     // the rows of the matrix are separated by dot-comma's
//                     while (this.isToken(';')) { // eslint-disable-line no-unmodified-loop-condition
//                         this.getToken()

//                         params[rows] = this.parseRow()
//                         rows++
//                     }

//                     if (!this.isToken(']')) {
//                         throw this.createSyntaxError('End of matrix ] expected')
//                     }
//                     this.closeParams()
//                     this.getToken()

//                     // check if the number of columns matches in all rows
//                     cols = params[0].items.length
//                     for (let r = 1; r < rows; r++) {
//                         if (params[r].items.length !== cols) {
//                             throw this.createError('Column dimensions mismatch ' +
//                                 '(' + params[r].items.length + ' !== ' + cols + ')')
//                         }
//                     }

//                     array = new ArrayNode(params)
//                 } else {
//                     // 1 dimensional vector
//                     if (!this.isToken(']')) {
//                         throw this.createSyntaxError('End of matrix ] expected')
//                     }
//                     this.closeParams()
//                     this.getToken()

//                     array = row
//                 }
//             } else {
//                 // this is an empty matrix "[ ]"
//                 this.closeParams()
//                 this.getToken()
//                 array = new ArrayNode([])
//             }

//             return this.parseAccessors(array)
//         }

//         return this.parseObject()
//     }

//     /**
//      * Parse a single comma-separated row from a matrix, like 'a, b, c'
//      * @return {ArrayNode} node
//      */
//     parseRow() {
//         const params = [this.parseAssignment()]
//         let len = 1

//         while (this.state.token === ',') { // eslint-disable-line no-unmodified-loop-condition
//             this.getToken()

//             // parse expression
//             params[len] = this.parseAssignment()
//             len++
//         }

//         return new ArrayNode(params)
//     }

//     /**
//      * parse an object, enclosed in angle brackets{...}, for example {value: 2}
//      * @return {Node} node
//      * @private
//      */
//     parseObject() {
//         if (this.state.token === '{') {
//             this.openParams()
//             let key

//             const properties: any = {}
//             do {
//                 this.getToken()

//                 if (!this.isToken('}')) {
//                     // parse key
//                     if (this.isToken('"')) {
//                         key = this.parseDoubleQuotesStringToken()
//                     } else if (this.isToken('\'')) {
//                         key = this.parseSingleQuotesStringToken()
//                     } else if (this.state.tokenType === TOKENTYPE.SYMBOL || (this.state.tokenType === TOKENTYPE.DELIMITER && this.state.token in NAMED_DELIMITERS)) {
//                         key = this.state.token
//                         this.getToken()
//                     } else {
//                         throw this.createSyntaxError('Symbol or string expected as object key')
//                     }

//                     // parse key/value separator
//                     if (!this.isToken(':')) {
//                         throw this.createSyntaxError('Colon : expected after object key')
//                     }
//                     this.getToken()

//                     // parse key
//                     properties[key] = this.parseAssignment()
//                 }
//             }
//             while (this.isToken(',')) // eslint-disable-line no-unmodified-loop-condition

//             if (!this.isToken('}')) {
//                 throw this.createSyntaxError('Comma , or bracket } expected after object value')
//             }
//             this.closeParams()
//             this.getToken()

//             let node = new ObjectNode(properties)

//             // parse index parameters
//             node = this.parseAccessors(node)

//             return node
//         }

//         return this.parseNumber()
//     }

//     /**
//      * parse a number
//      * @return {Node} node
//      * @private
//      */
//     parseNumber() {
//         let numberStr

//         if (this.state.tokenType === TOKENTYPE.NUMBER) {
//             // this is a number
//             numberStr = this.state.token
//             this.getToken()

//             return new ConstantNode(numeric(numberStr, config.number))
//         }

//         return this.parseParentheses()
//     }

//     /**
//      * parentheses
//      * @return {Node} node
//      * @private
//      */
//     parseParentheses() {
//         let node

//         // check if it is a parenthesized expression
//         if (this.state.token === '(') {
//             // parentheses (...)
//             this.openParams()
//             this.getToken()

//             node = this.parseAssignment() // start again

//             if (!this.isToken(')')) {
//                 throw this.createSyntaxError('Parenthesis ) expected')
//             }
//             this.closeParams()
//             this.getToken()

//             node = new ParenthesisNode(node)
//             node = this.parseAccessors(node)
//             return node
//         }

//         return this.parseEnd()
//     }

//     /**
//      * Evaluated when the expression is not yet ended but expected to end
//      * @return {Node} res
//      * @private
//      */
//     parseEnd() {
//         if (this.isToken('')) {
//             // syntax error or unexpected end of expression
//             throw this.createSyntaxError('Unexpected end of expression')
//         } else {
//             throw this.createSyntaxError('Value expected')
//         }
//     }

//     /**
//      * Shortcut for getting the current row value (one based)
//      * Returns the line of the currently handled expression
//      * @private
//      */
//     /* TODO: implement keeping track on the row number
//     row () {
//       return null
//     }
//     */

//     /**
//      * Shortcut for getting the current col value (one based)
//      * Returns the column (position) where the last this.state.token starts
//      * @private
//      */
//     col() {
//         return this.state.index - this.state.token.length + 1
//     }

//     /**
//      * Create an error
//      * @param {string} message
//      * @return {SyntaxError} instantiated error
//      * @private
//      */
//     createSyntaxError(message: string) {
//         const c = this.col()
//         const error = new SyntaxError(message + ' (char ' + c + ')')
//         error.char = c

//         return error
//     }

//     /**
//      * Create an error
//      * @param {string} message
//      * @return {Error} instantiated error
//      * @private
//      */
//     createError(message: string) {
//         const c = this.col()
//         const error = new SyntaxError(message + ' (char ' + c + ')')
//         error.char = c

//         return error
//     }

//     // // Now that we can parse, automatically convert strings to Nodes by parsing
//     // typed.addConversion({ from: 'string', to: 'Node', convert: parse })

//     // return parse
//     //     })


// }