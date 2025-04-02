import { match } from 'ts-pattern'

/**************************
        Raw Value
***************************/

export type Value = BoolVal

export type BoolVal = TrueVal | FalseVal | UnknownVal

interface BoolV {
  $type: 'TrueVal' | 'FalseVal' | 'UnknownVal'
  getClasses(): string[]
}

export class TrueVal implements BoolV {
  $type: 'TrueVal' = 'TrueVal' as const
  constructor() {}

  getClasses() {
    return ['true-val']
  }
}

export class FalseVal implements BoolV {
  $type: 'FalseVal' = 'FalseVal' as const
  constructor() {}

  getClasses() {
    return ['false-val']
  }
}

export class UnknownVal implements BoolV {
  $type: 'UnknownVal' = 'UnknownVal' as const
  constructor() {}

  getClasses() {
    return []
  }
}

export function cycle(val: BoolVal): BoolVal {
  return match(val)
    .with({ $type: 'TrueVal' }, () => new FalseVal())
    .with({ $type: 'FalseVal' }, () => new UnknownVal())
    .with({ $type: 'UnknownVal' }, () => new TrueVal())
    .exhaustive()
}

/**************************
       Result Value
***************************/

export class ResultValue {
  constructor(private readonly rawValue: Value) {}

  getClasses() {
    // TODO: Will prob wnat to add more
    return this.rawValue.getClasses()
  }
}
