import { hasOwnProperty } from "../../utils/object";
import { ConstantNode } from "../../node/ConstantNode";
import { ExpressionNode } from "../../node/ExpressionNode"
import { SymbolNode } from "../../node/SymbolNode";
import { State } from "../State"
import { CONSTANTS } from "../constants";
import { parseAccessors } from "./parseAccessors";
import { parseArray } from "./parseArray";

/**
  * parse symbols: functions, variables, constants, units
  * @return {ExpressionNode} node
  */
export function parseSymbol(state: State): ExpressionNode {
    let node, name

    if (state.isType("SYMBOL")) {
        name = state.token.value;


        if (hasOwnProperty(CONSTANTS, name)) { // true, false, null, ...
            node = new ConstantNode(CONSTANTS[name])
            // } else if (NUMERIC_CONSTANTS.indexOf(name) !== -1) { // NaN, Infinity
            //     node = new ConstantNode(numeric(name, 'number'))
        } else {
            node = new SymbolNode(name)
        }

        state.goAhead();

        // parse function parameters and matrix index
        node = parseAccessors(state, node)
        return node;
    }

    return parseArray(state)
}