import { setSafeProperty, hasSafeProperty, getSafeProperty } from './customs'
import { isObject } from './is'

/**
 * A map facade on a bare object.
 *
 * The small number of methods needed to implement a scope,
 * forwarding on to the SafeProperty functions. Over time, the codebase
 * will stop using this method, as all objects will be Maps, rather than
 * more security prone objects.
 */
export class ObjectWrappingMap {
  wrappedObject: object;
  constructor(object: object) {
    this.wrappedObject = object
  }

  keys() { return Object.keys(this.wrappedObject) }

  get(key: string) { return getSafeProperty(this.wrappedObject, key) }

  set(key: string, value: any) {
    setSafeProperty(this.wrappedObject, key, value)
    return this;
  }

  has(key: string) {
    return hasSafeProperty(this.wrappedObject, key)
  }
}

/**
 * Creates an empty map, or whatever your platform's polyfill is.
 *
 * @returns an empty Map or Map like object.
 */
export function createEmptyMap() {
  return new Map()
}

/**
 * Creates a Map from the given object.
 *
 * @param { Map | { [key: string]: unknown } | undefined } mapOrObject
 * @returns
 */
export function createMap(mapOrObject: Map<string, unknown> | { [key: string]: unknown } | undefined) {
  if (!mapOrObject) {
    return createEmptyMap()
  }
  if (isMap(mapOrObject)) {
    return mapOrObject
  }
  if (isObject(mapOrObject)) {
    return new ObjectWrappingMap(mapOrObject)
  }

  throw new Error('createMap can create maps from objects or Maps')
}

/**
 * Unwraps a map into an object.
 *
 * @param {Map} map
 * @returns { [key: string]: unknown }
 */
// export function toObject(map: ObjectWrappingMap | Map<string, any>) {
//   if (map instanceof ObjectWrappingMap) {
//     return map.wrappedObject
//   }
//   const object = {}
//   for (const key of (map.keys()) {
//     const value = map.get(key)
//     setSafeProperty(object, key, value)
//   }
//   return object
// }

/**
 * Returns `true` if the passed object appears to be a Map (i.e. duck typing).
 *
 * Methods looked for are `get`, `set`, `keys` and `has`.
 *
 * @param {Map | object} object
 * @returns
 */
export function isMap(object: any) {
  // We can use the fast instanceof, or a slower duck typing check.
  // The duck typing method needs to cover enough methods to not be confused with DenseMatrix.
  if (!object) {
    return false
  }
  return object instanceof Map ||
    object instanceof ObjectWrappingMap ||
    (
      typeof object.set === 'function' &&
      typeof object.get === 'function' &&
      typeof object.keys === 'function' &&
      typeof object.has === 'function'
    )
}

/**
 * Copies the contents of key-value pairs from each `objects` in to `map`.
 *
 * Object is `objects` can be a `Map` or object.
 *
 * This is the `Map` analog to `Object.assign`.
 */
export function assign(map: ObjectWrappingMap, ...objects: Array<ObjectWrappingMap>) {
  for (const args of objects) {
    if (!args) {
      continue
    }
    if (isMap(args)) {
      for (const key of args.keys()) {
        map.set(key, args.get(key))
      }
    } else if (isObject(args)) {
      for (const key of Object.keys(args)) {
        map.set(key, (args as any)[key])
      }
    }
  }
  return map
}
