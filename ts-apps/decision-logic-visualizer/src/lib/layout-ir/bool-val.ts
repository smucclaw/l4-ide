export type BoolVal = TrueVal | FalseVal | UnknownVal

interface BoolV {
  getStyles(): string
}

export class TrueVal implements BoolV {
  constructor() {}

  getStyles() {
    return 'background-color: var(--color-true-value);'
  }
}

export class FalseVal implements BoolV {
  constructor() {}

  getStyles() {
    return 'background-color: var(--color-false-value);'
  }
}

export class UnknownVal implements BoolV {
  constructor() {}

  getStyles() {
    return ''
  }
}
