import { Parser } from "../src/expression/parser/Parser";

const expr1 = "2+2";

describe("Testing the Parser", () => {

    test("test the tree", () => {

        console.log("Tree", new Parser().parseStart(expr1));

    })

});