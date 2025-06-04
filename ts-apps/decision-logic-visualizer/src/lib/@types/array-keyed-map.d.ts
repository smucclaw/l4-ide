/**************************************************
   Type definitions for array-keyed-map library
 **************************************************/

declare module 'array-keyed-map' {
  // TKeyArray is the type of the key array itself (e.g., string[], number[], LirId[]).
  // V is the type of the value.
  export default class ArrayKeyedMap<TKeyArray extends unknown[], V> {
    constructor(initialEntries?: Array<[TKeyArray, V]>)

    set(path: TKeyArray, value: V): this
    has(path: TKeyArray): boolean
    get(path: TKeyArray): V | undefined
    delete(path: TKeyArray): boolean
    clear(): void
    hasPrefix(path: TKeyArray): boolean

    readonly size: number;

    [Symbol.iterator](): Iterator<[TKeyArray, V]>
    entries(): Iterator<[TKeyArray, V]>
    keys(): Iterator<TKeyArray>
    values(): Iterator<V>
    forEach(
      callback: (
        value: V,
        key: TKeyArray,
        map: ArrayKeyedMap<TKeyArray, V>
      ) => void,
      thisArg?: object
    ): void
  }
}
