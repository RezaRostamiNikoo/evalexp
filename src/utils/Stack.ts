import { IStack } from "../interfaces/IStack";

export class Stack<T> implements IStack<T> {


    private items: Array<T> = [];

    constructor(...items: Array<T>) {
        if (items.length) this.items = items;
    }

    push(...items: Array<T>): number {
        return this.items.push(...items);
    }

    peek() {
        return this.items[this.items.length - 1];
    }

    isEmpty(): boolean {
        return this.items.length == 0;
    }

    pop(): T {
        if (!this.items.length) return undefined;
        return this.items.pop();
    }

    forEach(callbackfn: (value: T, index: number, array: T[]) => void, thisArg?: any): void {
        this.items.forEach(callbackfn);
    }

    map<U>(callbackfn: (value: T, index: number, array: T[]) => U, thisArg?: any): U[] {
        return this.items.map<U>(callbackfn);
    }

    at(index: number): T | undefined {
        return this.items[index];
    };
}