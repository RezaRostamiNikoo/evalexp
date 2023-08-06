export class Scope {
    data: Map<string, any>;
    constructor(scope: Map<string, any>, getValue: (item: any) => any, setValue: (item: any) => void) {
        this.data = scope;
    }



}