import { IScope } from "../src";

const data = new Map()
data.set("q", "12")
data.set("a", "1+2")
data.set("b", "a")
data.set("c", "b")
data.set("hamid", "c * b^2 / q * 4")
data.set("pp", "3.14")
data.set("pi", "1.57")
data.set("oo", {
    a: 125, b: 20, c: {
        a: "pp",
        b: [888888888, 999999999]
    }
})
data.set("aa", [400, 500, 600])

export class MScope implements IScope {
    getValue(key: string) { return data.get(key) }
    has(key: any): boolean { return data.has(key) }
}