
import { Tokenizer } from "../src";
import { sample1 } from "./_samples";

describe("Tokenizer Class Test", () => {

    test("State", () => {

        // "12.005 * (x+12)/(135e-3) + 12 - alignT(hamid_,jakj,12-0xde334a) > .EQ. < <= >= .GT. .NE. .LT. .GE. .LE."
        const t = new Tokenizer(sample1);

        expect(t.getNextToken().value).toEqual("12.005")
        expect(t.getNextToken().value).toEqual("*")
        expect(t.getNextToken().value).toEqual("(");
        expect(t.getNextToken().value).toEqual("x");
        expect(t.getNextToken().value).toEqual("+");
        expect(t.getNextToken().value).toEqual("12");
        expect(t.getNextToken().value).toEqual(")");
        expect(t.getNextToken().value).toEqual("/");
        expect(t.getNextToken().value).toEqual("(");
        expect(t.getNextToken().value).toEqual("135e-3");
        expect(t.getNextToken().value).toEqual(")");
        expect(t.getNextToken().value).toEqual("+");
        expect(t.getNextToken().value).toEqual("12");
        expect(t.getNextToken().value).toEqual("-");
        expect(t.getNextToken().value).toEqual("alignT");
        expect(t.getNextToken().value).toEqual("(");
        expect(t.getNextToken().value).toEqual("hamid_");
        expect(t.getNextToken().value).toEqual(",");
        expect(t.getNextToken().value).toEqual("jakj");
        expect(t.getNextToken().value).toEqual(",");
        expect(t.getNextToken().value).toEqual("12");
        expect(t.getNextToken().value).toEqual("-");
        expect(t.getNextToken().value).toEqual("0xde334a");
        expect(t.getNextToken().value).toEqual(")");
        expect(t.getNextToken().value).toEqual(">");
        expect(t.getNextToken().value).toEqual(".EQ.");
        expect(t.getNextToken().value).toEqual("<");
        expect(t.getNextToken().value).toEqual("<=");
        expect(t.getNextToken().value).toEqual(">=");
        expect(t.getNextToken().value).toEqual(".GT.");
        expect(t.getNextToken().value).toEqual(".NE.");
        expect(t.getNextToken().value).toEqual(".LT.");
        expect(t.getNextToken().value).toEqual(".GE.");
        expect(t.getNextToken().value).toEqual(".LE.");
    });
})
