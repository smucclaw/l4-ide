export type BoolVal = TrueVal | FalseVal | UnknownVal

interface BoolV {
  getClasses(): string[]
}

export class TrueVal implements BoolV {
  constructor() {}

  getClasses() {
    return ['true-val']
  }
}

export class FalseVal implements BoolV {
  constructor() {}

  getClasses() {
    return ['false-val']
  }
}

export class UnknownVal implements BoolV {
  constructor() {}

  getClasses() {
    return ['']
  }
}
