import { AccessorNode, ArrayNode, FunctionNode, IndexNode, OperatorNode, ParenthesesNode, RelationalNode, SymbolNode, parse } from "../src";
import { IScope } from "../src";
import { ConstantNode } from "../src"
import { isPlainObject } from "../src/utils/customs";
import { MScope } from "./data";



const scope = new MScope()

describe("Testing the ExpressionNodes", () => {

    test(`Test for "ConstantNode"`, () => {
        expect(new ConstantNode("458").evaluate()).toEqual(458)
    })

    test(`Test for "SymbolNode"`, () => {
        expect(new SymbolNode("q").evaluate(scope)).toEqual(12)
        expect(new SymbolNode("a").evaluate(scope)).toEqual(3)
        expect(new SymbolNode("hamid").evaluate(scope)).toEqual(9)
    })

    test(`Test for "FunctionNode"`, () => {
        expect(new FunctionNode("sin", [new SymbolNode("pp")]).evaluate(scope)).toEqual(Math.sin(3.14))
    })

    test(`Test for "AccessorNode"`, () => {

        expect(typeof parse("oo").evaluate(scope)).toEqual("object")
        expect(isPlainObject(parse("oo").evaluate(scope))).toEqual(true)
        expect(new AccessorNode(new SymbolNode("oo"), new IndexNode([new SymbolNode("a")], true)).evaluate(scope)).toEqual(125)

        expect(new AccessorNode(
            new AccessorNode(new SymbolNode("oo"), new IndexNode([new SymbolNode("c")], true)),
            new IndexNode([new SymbolNode("a")], true)
        ).evaluate(scope)).toEqual("pp")

        expect(new AccessorNode(
            new AccessorNode(
                new AccessorNode(new SymbolNode("oo"), new IndexNode([new SymbolNode("c")], true)),
                new IndexNode([new SymbolNode("b")], true)
            ), new IndexNode([new ConstantNode(1)])
        ).evaluate(scope)).toEqual(999999999)

    })

    test(`Test for "ArrayNode"`, () => {

        expect(new ArrayNode([
            new ConstantNode(1),
            new ConstantNode(2),
            new ConstantNode(3),
            new ConstantNode(4),
        ]).evaluate(scope)).toEqual([1, 2, 3, 4])

        expect(new ArrayNode([
            new ConstantNode(1),
            new ConstantNode(2),
            new ConstantNode(3),
            new ArrayNode([
                new ConstantNode(8),
                new ConstantNode(9),
                new SymbolNode("hamid")
            ])
        ]).evaluate(scope)).toEqual([1, 2, 3, [8, 9, 9]])
    })

    test(`Test for "OperatorNode"`, () => {
        expect(new OperatorNode("+", "add", [new ConstantNode(2), new ConstantNode(3)])
            .evaluate(scope)).toEqual(5)

        expect(new OperatorNode("-", "subtract", [new ConstantNode(2), new ConstantNode(3)])
            .evaluate(scope)).toEqual(-1)

        expect(new OperatorNode("-", "unaryMinus", [new ConstantNode(2)])
            .evaluate(scope)).toEqual(-2)

    })

    test(`Test for "ParenthesesNode"`, () => {
        expect(new ParenthesesNode(new ArrayNode([new ConstantNode(3)]))
            .evaluate(scope)).toEqual([3])
    })

    test(`Test for "RelationalNode"`, () => {
        expect(new RelationalNode(
            [
                "larger"
            ],
            [
                new ConstantNode(5),
                new ConstantNode(6)
            ]

        )
            .evaluate(scope)).toEqual(false)

        expect(new RelationalNode(
            [
                "smaller",
                "smaller"
            ],
            [
                new ConstantNode(5),
                new ConstantNode(6),
                new ConstantNode(10),
            ]

        )
            .evaluate(scope)).toEqual(true)
    })

});
