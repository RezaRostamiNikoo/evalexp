export const equal = (x: number, y: number) => x === y;
export const unequal = (x: number, y: number) => x !== y;
export const smaller = (x: number, y: number) => x < y;
export const larger = (x: number, y: number) => x > y;
export const smallerEq = (x: number, y: number) => x <= y;
export const largerEq = (x: number, y: number) => x >= y;
export const or = (x: number, y: number) => Boolean(x) || Boolean(y);
export const and = (x: number, y: number) => Boolean(x) && Boolean(y);
