import * as z from 'zod'
import type { FunctionEndpointInfo } from '../decision-api-client/api.js'
import { match, P } from 'ts-pattern'

/** Consruct a Zod schema for the function params corresponding to the input function endpoint */
export function makeFunctionParamSchema(functionInfo: FunctionEndpointInfo) {
  const properties = functionInfo.function?.parameters?.properties

  if (!properties) {
    throw new Error('No parameters defined for the function.')
  }

  const schemaShape = Object.fromEntries(
    Object.entries(properties).map(([paramName, paramInfo]) => {
      let paramSchema = match(paramInfo)
        .with(
          {
            type: 'string',
            enum: P.when((arr) => Array.isArray(arr) && arr.length > 0),
          },
          () => z.enum(paramInfo.enum as [string, ...string[]])
        )
        .with({ type: 'string' }, () => z.string())
        .with({ type: 'number' }, () => z.number())
        .with({ type: 'boolean' }, () => z.boolean())
        .with({ type: 'array' }, () => z.array(z.any()))
        .otherwise(() => {
          throw new Error(`Unsupported parameter type: ${paramInfo.type}`)
        })

      // Add metadata like description
      if (paramInfo.description) {
        paramSchema = paramSchema.describe(paramInfo.description)
      }

      // Ensure these property is required by not adding `.optional()`
      // Since Zod properties are required by default, we don't need to do anything extra

      return [paramName, paramSchema]
    })
  )

  // Create the final Zod schema object
  return z.object(schemaShape)
}
