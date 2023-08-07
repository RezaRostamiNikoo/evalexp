export interface IStack<T> {

    /**
     * push element into the items
     */
    push(item: T);


    /**
     * return top most element in the stack
     * and removes it from the stack
     * Underflow if stack is empty
     */
    pop(): T


    /**
     * return the top most element from the stack
     * but does'nt delete it.     
     */
    peek()

    /**
     * 
     * @returns {boolean} return boolean
     */
    isEmpty(): boolean;


    /**
     * it return the legth of Stack
     * @returns {number} returns number
     */
    get length(): number;


    forEach(callbackfn: (value: T, index: number, array: T[]) => void, thisArg?: any): void;

    map<U>(callbackfn: (value: T, index: number, array: T[]) => U, thisArg?: any): U[];

    at(index: number): T | undefined;


    reduce(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T): T;
}