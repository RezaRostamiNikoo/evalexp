import { IScopeItem } from './IScopeItem'

export interface IScope {
    get(key: string): IScopeItem
    set(key: string, value: IScopeItem)
    has(key: any): boolean

    forEach(callback: (value: IScopeItem, key: string) => void): void
    keys(): IterableIterator<string>

    
}


