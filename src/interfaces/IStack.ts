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
}