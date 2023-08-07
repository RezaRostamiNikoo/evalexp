
import { State } from "../src/expression/parser/State";

describe("State", () => {

    test("State", () => {

        const s = new State("12.005 * (x+12)/(135e-3) + 12 - alignT(hamid_,jakj,12-0xde334a) > .EQ. < <= >=`");
        expect(s.token.Value).toEqual("12.005");
        expect(s.goAHead().token.Value).toEqual("*");
        expect(s.goAHead().token.Value).toEqual("(");
        expect(s.goAHead().token.Value).toEqual("x");
        expect(s.goAHead().token.Value).toEqual("+");
        expect(s.goAHead().token.Value).toEqual("12");
        expect(s.goAHead().token.Value).toEqual(")");
        expect(s.goAHead().token.Value).toEqual("/");
        expect(s.goAHead().token.Value).toEqual("(");
        expect(s.goAHead().token.Value).toEqual("135e-3");
        expect(s.goAHead().token.Value).toEqual(")");
        expect(s.goAHead().token.Value).toEqual("+");
        expect(s.goAHead().token.Value).toEqual("12");
        expect(s.goAHead().token.Value).toEqual("-");
        expect(s.goAHead().token.Value).toEqual("alignT");
        expect(s.goAHead().token.Value).toEqual("(");
        expect(s.goAHead().token.Value).toEqual("hamid_");
        expect(s.goAHead().token.Value).toEqual(",");
        expect(s.goAHead().token.Value).toEqual("jakj");
        expect(s.goAHead().token.Value).toEqual(",");
        expect(s.goAHead().token.Value).toEqual("12");
        expect(s.goAHead().token.Value).toEqual("-");
        expect(s.goAHead().token.Value).toEqual("0xde334a");
        expect(s.goAHead().token.Value).toEqual(")");
        expect(s.goAHead().token.Value).toEqual(">");
        expect(s.goAHead().token.Value).toEqual(".EQ.");
        expect(s.goAHead().token.Value).toEqual("<");
        expect(s.goAHead().token.Value).toEqual("<=");
        expect(s.goAHead().token.Value).toEqual(">=");
    });

    test("State", () => {

        const s = new State("12 + 3");
        expect(s.token.Value).toEqual("12");
        expect(s.goAHead().token.Value).toEqual("+");
        expect(s.goAHead().token.Value).toEqual("3");

    });

})
