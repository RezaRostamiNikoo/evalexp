
import { State } from "../src";
import { sample1 } from "./_samples";

describe("State", () => {

    test("State", () => {

        // "12.005 * (x+12)/(135e-3) + 12 - alignT(hamid_,jakj,12-0xde334a) > .EQ. < <= >=`"
        const s = new State(sample1);
        expect(s.token.value).toEqual("12.005");
        expect(s.goAhead().token.value).toEqual("*");
        expect(s.goAhead().token.value).toEqual("(");
        expect(s.goAhead().token.value).toEqual("x");
        expect(s.goAhead().token.value).toEqual("+");
        expect(s.goAhead().token.value).toEqual("12");
        expect(s.goAhead().token.value).toEqual(")");
        expect(s.goAhead().token.value).toEqual("/");
        expect(s.goAhead().token.value).toEqual("(");
        expect(s.goAhead().token.value).toEqual("135e-3");
        expect(s.goAhead().token.value).toEqual(")");
        expect(s.goAhead().token.value).toEqual("+");
        expect(s.goAhead().token.value).toEqual("12");
        expect(s.goAhead().token.value).toEqual("-");
        expect(s.goAhead().token.value).toEqual("alignT");
        expect(s.goAhead().token.value).toEqual("(");
        expect(s.goAhead().token.value).toEqual("hamid_");
        expect(s.goAhead().token.value).toEqual(",");
        expect(s.goAhead().token.value).toEqual("jakj");
        expect(s.goAhead().token.value).toEqual(",");
        expect(s.goAhead().token.value).toEqual("12");
        expect(s.goAhead().token.value).toEqual("-");
        expect(s.goAhead().token.value).toEqual("0xde334a");
        expect(s.goAhead().token.value).toEqual(")");
        expect(s.goAhead().token.value).toEqual(">");
        expect(s.goAhead().token.value).toEqual(".EQ.");
        expect(s.goAhead().token.value).toEqual("<");
        expect(s.goAhead().token.value).toEqual("<=");
        expect(s.goAhead().token.value).toEqual(">=");
    });
})
