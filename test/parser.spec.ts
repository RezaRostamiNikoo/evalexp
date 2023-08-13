import { Parser } from "../src/expression/parser";


describe("Testing the Parser", () => {

    test("Addition operator", () => {
        expect(new Parser().parseStart("12 + 65").toString()).toEqual("12 + 65")
    })

    test("multiply operation", () => {
        expect(new Parser().parseStart("12 * 65").toString()).toEqual("12 * 65")
    })

    test("subtract operation", () => {
        expect(new Parser().parseStart("12 -     65").toString()).toEqual("12 - 65")
    })

    test("deviding operation", () => {
        expect(new Parser().parseStart("12 / 65").toString()).toEqual("12 / 65")
    })
    test("unary", () => {
        expect(new Parser().parseStart("12 / -65").toString()).toEqual("12 / -65")
    })
    test("parantheses", () => {
        expect(new Parser().parseStart("(15 +     63)").toString()).toEqual("(15 + 63)")
        expect(new Parser().parseStart("(15 + 63 * (24 / 5))").toString()).toEqual("(15 + 63 * (24 / 5))")
    })

    test("power sign", () => {
        expect(new Parser().parseStart("6^5").toString()).toEqual("6 ^ 5")
        expect(new Parser().parseStart("6^-6").toString()).toEqual("6 ^ -6")
    });

    test("function", () => {
        expect(new Parser().parseStart("hamid(ahmad)").toString()).toEqual("hamid(ahmad)")
        expect(new Parser().parseStart("hamid(ahmad  ,   usef)").toString()).toEqual("hamid(ahmad, usef)")
        expect(new Parser().parseStart("6^5 + (hamid() / (33 + hamid(ahmad  ,   usef)))").toString())
            .toEqual("6 ^ 5 + (hamid() / (33 + hamid(ahmad, usef)))")
    })


    test("Array", () => {
        expect(new Parser().parseStart("[1,2,3]").toString()).toEqual("[1, 2, 3]")
        expect(new Parser().parseStart("[hamid(),5^3,24/6]").toString()).toEqual("[hamid(), 5 ^ 3, 24 / 6]")
    })

    test("Rational", () => {
        expect(new Parser().parseStart("3 .EQ. 5").toString()).toEqual("3 .EQ. 5")
        expect(new Parser().parseStart("hamid.EQ.5").toString()).toEqual("hamid .EQ. 5")
        // expect(new Parser().parseStart("3.EQ. 5").toString()).toEqual("3 .EQ. 5")
    });

    test("Accessor", () => {
        expect(new Parser().parseStart("hamid  .  reza  .  f").toString()).toEqual("hamid.reza.f")
        expect(new Parser().parseStart("  hamid  [  reza  ]   [  f   ]  ").toString()).toEqual("hamid[reza][f]")
    });


    test("complex", () => {
        // const expr = "2^-3 * -sin(12(23/2x)) * -hamid.reza fn(12/5(sin(0)),-45.35,-heded[1])";
        // expect(new Parser().parseStart(expr).toString())
        //     .toEqual("2 ^ -3 * -sin(12 * (23 / (2 * x))) * -hamid.reza fn(12 / (5 * (sin(0))),-45.35,-heded[1])")

        const expr = "12/2x";
        expect(new Parser().parseStart(expr).toString())
            .toEqual("12 / (2 * x)");


    });
});