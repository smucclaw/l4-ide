/**************************************************
   Type definitions for array-keyed-map library
 **************************************************/

declare module 'array-keyed-map' {
  export default class ArrayKeyedMap<K, V> {
    constructor(initialEntries?: Array<[K[], V]>)

    set(path: K[], value: V): this
    has(path: K[]): boolean
    get(path: K[]): V | undefined
    delete(path: K[]): boolean
    clear(): void
    hasPrefix(path: K[]): boolean

    readonly size: number;

    [Symbol.iterator](): Iterator<[K[], V]>
    entries(): Iterator<[K[], V]>
    keys(): Iterator<K[]>
    values(): Iterator<V>
    forEach(
      callback: (value: V, key: K[], map: ArrayKeyedMap<K, V>) => void,
      thisArg?: object
    ): void
  }
}
