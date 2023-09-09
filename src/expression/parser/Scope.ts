export class Scope {
    private data: Map<string, any>;
    private getValue: (key: any) => any;
    private setValue: (key: string, value: any) => void;

    constructor(scope?: Map<string, any>, getValue?: (key: any) => any, setValue?: (key: string, value: any) => void) {
        this.data = scope || new Map();
    }

    get(key: any): any {
        return this.getValue ? this.getValue(key) : this.data.get(key);
    }
    set(key: string, value: any): this {
        this.setValue ? this.setValue(key, value) : this.data.set(key, value);
        return this;
    }

    has(key: any): boolean { return this.data.has(key); }
}