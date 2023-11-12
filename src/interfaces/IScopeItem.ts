export interface IScopeItem {
    hasBeenCalculated(): boolean
    getRawValue(): any
    getCalculatedValue(): any
    setCalculatedValue(value: any): void
}