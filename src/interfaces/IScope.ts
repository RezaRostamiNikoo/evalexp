import { IScopeItem } from './IScopeItem'

export interface IScope {

    getOwnItems(): Map<string, IScopeItem>
    getOtherItems(): Map<string, IScopeItem>
    getItems(): Map<string, IScopeItem>

    hasOwn(key: string): boolean
    hasOther(key: string): boolean
    has(key: string): boolean

    getItem(key: string): IScopeItem
    getItemFromOwn(key: string): IScopeItem
    getItemFromOther(key: string): IScopeItem

    getvalueFromOwn(key: string): any
    getValueFromOther(key: string): any
    getValue(key: string): any

    setToOwn(key: string, item: IScopeItem): this
    setToOther(key: string, item: IScopeItem): this

    removeFromOwn(key: string): IScopeItem
    removeFromOther(key: string): IScopeItem
    removeItem(key: string): IScopeItem

    resetOwnItem(key: string): any
    resetOtherItem(key: string): any
    resetItem(key: string): any

    resetOwnItems(): void
    resetOtherItems(): void
    resetItems(): void

    ownToJson(): Object
    otherToJson(): Object
    toJson(): Object

    emptyOwn(): void
    emptyOther(): void
    empty(): void

    forEach(callback: (value: IScopeItem, key: string) => void): void
    keys(): IterableIterator<string>

    calculateOwnItem(key: string): any
    calculateOtherItem(key: string): any
    calculateItem(key: string): any
    calculateOwn(): this
    calculateOther(): this
    calculate(): this
}


