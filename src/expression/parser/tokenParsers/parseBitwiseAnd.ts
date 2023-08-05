import { State } from "../State"
import { parseRelational } from "./parseRelational"

/**
  * bitwise and, 'x & y'
  * @return {Node} node
  * @private
  */
export function parseBitwiseAnd(state: State) {
 return  parseRelational(state)

}

