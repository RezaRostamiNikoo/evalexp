
import { State } from "../src/expression/parser/State";
test("State", () => {

    const s = new State("12.005 * (x+12)/(135e3) + 12 - alignT(hamid_,jakj,12-0xde334a) > .EQ. < <= >=");
    expect(s.getToken()).toEqual("12.005");
    expect(s.getToken()).toEqual("*");
    expect(s.getToken()).toEqual("(");
    expect(s.getToken()).toEqual("x");
    expect(s.getToken()).toEqual("+");
    expect(s.getToken()).toEqual("12");
    expect(s.getToken()).toEqual(")");
    expect(s.getToken()).toEqual("/");
    expect(s.getToken()).toEqual("(");
    expect(s.getToken()).toEqual("135e3");
    expect(s.getToken()).toEqual(")");
    expect(s.getToken()).toEqual("+");
    expect(s.getToken()).toEqual("12");
    expect(s.getToken()).toEqual("-");
    expect(s.getToken()).toEqual("alignT");
    expect(s.getToken()).toEqual("(");
    expect(s.getToken()).toEqual("hamid_");
    expect(s.getToken()).toEqual(",");
    expect(s.getToken()).toEqual("jakj");
    expect(s.getToken()).toEqual(",");
    expect(s.getToken()).toEqual("12");
    expect(s.getToken()).toEqual("-");
    expect(s.getToken()).toEqual("0xde334a");
    expect(s.getToken()).toEqual(")");
    expect(s.getToken()).toEqual(">");
    expect(s.getToken()).toEqual(".EQ.");
    expect(s.getToken()).toEqual("<");
    expect(s.getToken()).toEqual("<=");
    expect(s.getToken()).toEqual(">=");
});