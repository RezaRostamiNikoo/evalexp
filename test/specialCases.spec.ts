import { parse } from "../src"

describe("special cases", () => {

    test('some special cases', () => {

        expect(parse(12 as unknown as string)).toEqual(12)
        expect(parse({ a: 1 } as unknown as string)).toEqual({ a: 1 })
        expect(parse([1] as unknown as string)).toEqual([1])
        expect(parse("0").evaluate()).toEqual(0)

    })

})