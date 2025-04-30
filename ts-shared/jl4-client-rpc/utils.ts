/** This differs from VSCode's Disposable in that the return type can be Promise<void> */
export interface IAmDisposable {
  dispose(): void | Promise<void>
}
