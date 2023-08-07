
import { State } from "../src/expression/parser/State";

describe("State", () => {

    test("State", () => {

        const s = new State("12.005 * (x+12)/(135e3) + 12 - alignT(hamid_,jakj,12-0xde334a) > .EQ. < <= >=`");
        const tokens = s.Tokens;
        expect(tokens.dequeue().Value).toEqual("12.005");
        expect(tokens.dequeue().Value).toEqual("*");
        expect(tokens.dequeue().Value).toEqual("(");
        expect(tokens.dequeue().Value).toEqual("x");
        expect(tokens.dequeue().Value).toEqual("+");
        expect(tokens.dequeue().Value).toEqual("12");
        expect(tokens.dequeue().Value).toEqual(")");
        expect(tokens.dequeue().Value).toEqual("/");
        expect(tokens.dequeue().Value).toEqual("(");
        expect(tokens.dequeue().Value).toEqual("135e-3");
        expect(tokens.dequeue().Value).toEqual(")");
        expect(tokens.dequeue().Value).toEqual("+");
        expect(tokens.dequeue().Value).toEqual("12");
        expect(tokens.dequeue().Value).toEqual("-");
        expect(tokens.dequeue().Value).toEqual("alignT");
        expect(tokens.dequeue().Value).toEqual("(");
        expect(tokens.dequeue().Value).toEqual("hamid_");
        expect(tokens.dequeue().Value).toEqual(",");
        expect(tokens.dequeue().Value).toEqual("jakj");
        expect(tokens.dequeue().Value).toEqual(",");
        expect(tokens.dequeue().Value).toEqual("12");
        expect(tokens.dequeue().Value).toEqual("-");
        expect(tokens.dequeue().Value).toEqual("0xde334a");
        expect(tokens.dequeue().Value).toEqual(")");
        expect(tokens.dequeue().Value).toEqual(">");
        expect(tokens.dequeue().Value).toEqual(".EQ.");
        expect(tokens.dequeue().Value).toEqual("<");
        expect(tokens.dequeue().Value).toEqual("<=");
        expect(tokens.dequeue().Value).toEqual(">=");
    });

    test("State", () => {

        const s = new State("12 + 3");
        const tokens = s.Tokens;
        expect(tokens.dequeue().Value).toEqual("12");
        expect(tokens.dequeue().Value).toEqual("+");
        expect(tokens.dequeue().Value).toEqual("3");

    });

})
