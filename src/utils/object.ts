

/**
 * Deep test equality of all fields in two pairs of arrays or objects.
 * Compares values and functions strictly (ie. 2 is not the same as '2').
 * @param {Array | Object} a
 * @param {Array | Object} b
 * @returns {boolean}
 */
export function deepStrictEqual(a: Object, b: Object) {
  let prop, i, len
  if (Array.isArray(a)) {
    if (!Array.isArray(b)) {
      return false
    }

    if (a.length !== b.length) {
      return false
    }

    for (i = 0, len = a.length; i < len; i++) {
      if (!deepStrictEqual(a[i], b[i])) {
        return false
      }
    }
    return true
  } else if (typeof a === 'function') {
    return (a === b)
  } else if (a instanceof Object) {
    if (Array.isArray(b) || !(b instanceof Object)) {
      return false
    }

    for (prop in a) {
      // noinspection JSUnfilteredForInLoop
      if (!(prop in b) || !deepStrictEqual((a as any)[prop], (b as any)[prop])) {
        return false
      }
    }
    for (prop in b) {
      // noinspection JSUnfilteredForInLoop
      if (!(prop in a)) {
        return false
      }
    }
    return true
  } else {
    return (a === b)
  }
}


/**
 * A safe hasOwnProperty
 * @param {Object} object
 * @param {string} property
 */
export function hasOwnProperty(object: Object, property: string) {
  return object && Object.hasOwnProperty.call(object, property)
}
