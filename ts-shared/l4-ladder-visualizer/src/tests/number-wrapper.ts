import { ComparisonResult } from '../lib/utils.js'
import type { Ord, HasId } from '../lib/utils.js'

export class NumberWrapper implements Ord<NumberWrapper>, HasId {
  constructor(private value: number) {}

  isEqualTo(other: NumberWrapper): boolean {
    return other instanceof NumberWrapper && this.value === other.value
  }

  compare(other: NumberWrapper): ComparisonResult {
    if (this.value < other.value) return ComparisonResult.LessThan
    if (this.value > other.value) return ComparisonResult.GreaterThan
    return ComparisonResult.Equal
  }

  getValueAsNumber() {
    return this.value
  }

  getId(): string {
    return this.value.toString()
  }

  toString(): string {
    return `NWrapper ${this.value}`
  }
}
