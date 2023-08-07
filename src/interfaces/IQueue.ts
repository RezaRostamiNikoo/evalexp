export interface IQueue<T> {

    /**
     * adds elements to queue
     * @param {T} element 
     */
    enqueue(element: T);

    /**
     * removing element from the queue
     * returns undefined when called
     * on empty queue
     * @returns {T}
     */
    dequeue(): T;

    /**
     * returns the Front element of
     * the queue without removing it.
     * @returns {T} return T
     */
    peek(): T;

    /**
     * return true if the queue is empty.
     * @returns {boolean} return boolean
     */
    isEmpty(): boolean

    /**
     * it return the legth of Queue
     * @returns {number} returns number
     */
    get length(): number;

    forEach(callbackfn: (value: T, index: number, array: T[]) => void, thisArg?: any): void;

    map<U>(callbackfn: (value: T, index: number, array: T[]) => U, thisArg?: any): U[];

    at(index: number): T | undefined;

    reduce(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T): T;
}