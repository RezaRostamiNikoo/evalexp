import { parse } from "../src/expression/parser";


describe("Testing the Parser", () => {

    test("Addition operator", () => {
        expect(parse("12 + 65").toString()).toEqual("12 + 65")
    })

    test("multiply operation", () => {
        expect(parse("12 * 65").toString()).toEqual("12 * 65")
    })

    test("subtract operation", () => {
        expect(parse("12 -     65").toString()).toEqual("12 - 65")
    })

    test("deviding operation", () => {
        expect(parse("12 / 65").toString()).toEqual("12 / 65")
    })
    test("unary", () => {
        expect(parse("12 / -65").toString()).toEqual("12 / -65")
    })
    test("parantheses", () => {
        expect(parse("(15 +     63)").toString()).toEqual("(15 + 63)")
        expect(parse("(15 + 63 * (24 / 5))").toString()).toEqual("(15 + 63 * (24 / 5))")
    })

    test("power sign", () => {
        expect(parse("6^5").toString()).toEqual("6 ^ 5")
        expect(parse("6^-6").toString()).toEqual("6 ^ -6")
    });

    test("function", () => {
        expect(parse("hamid(ahmad)").toString()).toEqual("hamid(ahmad)")
        expect(parse("hamid(ahmad  ,   usef)").toString()).toEqual("hamid(ahmad, usef)")
        expect(parse("6^5 + (hamid() / (33 + hamid(ahmad  ,   usef)))").toString())
            .toEqual("6 ^ 5 + (hamid() / (33 + hamid(ahmad, usef)))")
    })


    test("Array", () => {
        expect(parse("[1,2,3]").toString()).toEqual("[1, 2, 3]")
        expect(parse("[hamid(),5^3,24/6]").toString()).toEqual("[hamid(), 5 ^ 3, 24 / 6]")
    })

    test("Rational", () => {
        expect(parse("3 .EQ. 5").toString()).toEqual("3 .EQ. 5")
        expect(parse("hamid.EQ.5").toString()).toEqual("hamid .EQ. 5")
        // expect(parse("3.EQ. 5").toString()).toEqual("3 .EQ. 5")
    });

    test("Accessor", () => {
        expect(parse("hamid  .  reza  .  f").toString()).toEqual("hamid.reza.f")
        expect(parse("  hamid  [  reza  ]   [  f   ]  ").toString()).toEqual("hamid[reza][f]")
    });


    test("complex", () => {
        const expr = "2^-3 * -sin(12(23/2x)) * -hamid.reza fn(12/5(sin(0)),-45.35,-heded[1])";
        expect(parse(expr).toString())
            .toEqual("2 ^ -3 * -sin((12 * (23 / (2 * x)))) * (-hamid.reza * fn(12 / (5 * (sin(0))), -45.35, -heded[1]))")



    });
});