import { describe, expect, it } from 'vitest'
import { schemaAtPathVariants, sortAsksForElicitation } from '../query-plan.js'
import { schemaSummary } from '../schema.js'
import type { Parameter, Parameters, QueryAsk } from '../index.js'

const pBool: Parameter = {
  type: 'boolean',
  alias: null,
  enum: [],
  description: '',
}

describe('schemaAtPathVariants', () => {
  it('supports schema-aware longest-match for dotted property keys', () => {
    const root: Parameter = {
      type: 'object',
      alias: null,
      enum: [],
      description: '',
      properties: {
        'a.b': pBool,
        x: pBool,
      },
      propertyOrder: ['a.b', 'x'],
    }

    expect(schemaAtPathVariants(root, ['a', 'b'])?.type).toBe('boolean')
    expect(schemaAtPathVariants(root, ['x'])?.type).toBe('boolean')
  })
})

describe('sortAsksForElicitation', () => {
  it('orders array indices numerically and object fields by propertyOrder', () => {
    const recordItem: Parameter = {
      type: 'object',
      alias: null,
      enum: [],
      description: '',
      properties: {
        recordNumber: { ...pBool, type: 'number' },
        recordLabel: { ...pBool, type: 'string' },
      },
      propertyOrder: ['recordNumber', 'recordLabel'],
    }

    const listOfRecords: Parameter = {
      type: 'array',
      alias: null,
      enum: [],
      description: '',
      items: recordItem,
    }

    const rol: Parameter = {
      type: 'object',
      alias: null,
      enum: [],
      description: '',
      properties: {
        people: listOfRecords,
        wrappers: listOfRecords,
      },
      propertyOrder: ['people', 'wrappers'],
    }

    const params: Parameters = {
      type: 'object',
      properties: { rol },
      required: ['rol'],
    }

    const ask = (path: string[]): QueryAsk => ({
      container: 'rol',
      key: path.join('.'),
      path,
      label: `rol.${path.join('.')}`,
      score: 10,
      atoms: [],
      schema: null,
    })

    const sorted = sortAsksForElicitation(params, [
      ask(['people', '10', 'recordLabel']),
      ask(['people', '2', 'recordLabel']),
      ask(['people', '2', 'recordNumber']),
      ask(['people', '10', 'recordNumber']),
      ask(['wrappers', '1', 'recordLabel']),
    ])

    expect(sorted.map((a) => a.path)).toEqual([
      ['people', '2', 'recordNumber'],
      ['people', '2', 'recordLabel'],
      ['people', '10', 'recordNumber'],
      ['people', '10', 'recordLabel'],
      ['wrappers', '1', 'recordLabel'],
    ])
  })
})

describe('schemaSummary', () => {
  it('summarizes scalar, enum, list, and record shapes', () => {
    expect(schemaSummary(pBool)).toBe('Bool')
    expect(
      schemaSummary({ ...pBool, enum: ['Yes', 'No'], type: 'string' })
    ).toBe('enum{Yes|No}')
    expect(
      schemaSummary({
        ...pBool,
        enum: ['a', 'b', 'c', 'd', 'e'],
        type: 'string',
      })
    ).toBe('enum(5)')

    expect(schemaSummary({ ...pBool, type: 'array', items: pBool })).toBe(
      'List<Bool>'
    )

    const rec: Parameter = {
      type: 'object',
      alias: null,
      enum: [],
      description: '',
      properties: { z: pBool, a: pBool, b: pBool, c: pBool, d: pBool },
      propertyOrder: ['b', 'a', 'z', 'c', 'd'],
    }
    expect(schemaSummary(rec)).toBe('Record{b,a,z,c,…}')
  })
})
