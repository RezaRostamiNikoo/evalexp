export interface IScopeItem {
    isCalculated(): boolean
    getRawValue(): any
    getCalculatedValue(): any
    setCalculatedValue(value: any): void
    reset(): any
}