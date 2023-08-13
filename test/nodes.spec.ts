import { Parser } from "../src/expression/parser";


describe("Testing the Parser", () => {

    test("ConstantNode", () => {
        expect(new Parser().parseStart("12").evaluate()).toEqual(12)
    })

    test("SymbolNode", () => {
        expect(new Parser().parseStart("hamid").evaluate()).toEqual("hamid")
        expect(new Parser().parseStart("hamid").evaluate()).toEqual("hamid")
    })


});