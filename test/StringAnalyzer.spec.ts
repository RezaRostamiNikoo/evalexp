import { StringAnalyzer } from "../src/index";

const sample1 = "12 * (24^3 - 2E1.6 / X + hamid(1233))";

describe("StringAnalyzer Class Test", () => {

    test("", () => {
        const sa = new StringAnalyzer(sample1)
        expect(sa.head).toEqual(0)
        expect(sa.scoutForward(3).scout).toEqual(3)
        expect(sa.headForward(4).head).toEqual(4)
        expect(sa.scout).toEqual(4)

        expect(sa.resetAll().scout).toEqual(0)
        expect(sa.head).toEqual(0)

        sa.headForward(6)
        sa.scoutForward(3)
        expect(sa.charByHead(15)).toEqual("X")
        expect(sa.charByScout()).toEqual("3")

        expect(sa.charByScout(-2)).toEqual("4")

        expect(sa.resetAll().headForward(25).charByHead()).toEqual("h")
        expect(sa.chunkFromHead(5)).toEqual("hamid")
        expect(sa.scoutForward(6).charByScout()).toEqual("1")
        expect(sa.chunkfromScout(4)).toEqual("1233")
        expect(sa.chunkHeadToScout()).toEqual("hamid(")

        expect(sa.tokenizeHeadToScout()).toEqual("hamid(")
        expect(sa.head).toEqual(31)
        expect(sa.charByHead()).toEqual("1")
        expect(sa.scout).toEqual(31)
        expect(sa.charByScout()).toEqual("1")

        expect(sa.isEndOfText()).toEqual(false);

        expect(sa.headForward(5).charByHead()).toEqual(")")
        expect(sa.remainedCharByHead()).toEqual(1)
        expect(sa.remainedCharByScout()).toEqual(1)

        expect(sa.isEndOfText()).toEqual(false);

        expect(sa.headForward(1).charByHead()).toEqual("")
        expect(sa.isEndOfText()).toEqual(true);


    })



})