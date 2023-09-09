import { parse } from "../src/expression/parser";
import { Scope } from "../src/expression/parser/Scope";
import * as functions from "../src/function"

const data = new Map();
data.set("a", "1+2");
data.set("b", "a");
data.set("c", "b");
data.set("hamid", "c");
const scope = new Scope(data);


describe("Testing the Parser", () => {

    test("simple", () => {
        expect(parse("12").evaluate()).toEqual(12)
    })

    test("SymbolNode", () => {
        expect(parse("12+3").evaluate(scope)).toEqual(15)
        expect(parse("12/3").evaluate(scope)).toEqual(4)
        expect(parse("12*3").evaluate(scope)).toEqual(36)
        expect(parse("12-3").evaluate(scope)).toEqual(9)
        expect(parse("2^3").evaluate(scope)).toEqual(8)
        expect(parse("2^-3").evaluate(scope)).toEqual(Math.pow(2, -3))
        expect(parse("2e3").evaluate(scope)).toEqual(2e3)
    })

    test("Functions", () => {
        expect(parse("sin(3.14)").evaluate(scope)).toEqual(Math.sin(3.14))

    })


});