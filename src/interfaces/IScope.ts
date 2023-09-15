export interface IScope {
    get(key: string): any
    getWithArgs(key: string, ...args: any[]): any
    set(key: string, value: any)
    has(key: any): boolean
}