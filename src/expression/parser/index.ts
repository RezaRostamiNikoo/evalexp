import { State } from "./State";


export function fdo(expression) {
    const state = new State(expression);
    state.getToken();
    console.log(state.token);
}