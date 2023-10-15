import { IQueue } from "../interfaces/IQueue";

export class Queue<T> implements IQueue<T>{
    private items: Array<T> = [];

    constructor(...items: Array<T>) {
        if (items.length) this.items = items;
    }

    isEmpty(): boolean { return this.items.length == 0; }

    enqueue(item: T) { this.items.push(item); }
    enqueueAtFirst(item: T) { this.items = [item, ...this.items]; }

    dequeue(): T { return this.items.shift(); }

    peek() { return this.items[0]; }

    forEach(callbackfn: (value: T, index: number, array: T[]) => void, thisArg?: any): void {
        this.items.forEach(callbackfn);
    }

    map<U>(callbackfn: (value: T, index: number, array: T[]) => U, thisArg?: any): U[] {
        return this.items.map<U>(callbackfn);
    }

    at(index: number): T | undefined {
        return this.items[index];
    };

    get length() { return this.items.length; }

    reduce(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T, initialValue?: unknown): T {
        return this.items.reduce(callbackfn);
    }

}
