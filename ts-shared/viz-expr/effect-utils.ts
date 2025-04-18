import { Schema } from 'effect'

export const Integer = Schema.Int.pipe(Schema.brand('Integer'))
