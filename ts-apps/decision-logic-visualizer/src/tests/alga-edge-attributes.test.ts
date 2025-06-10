import { describe, test, expect } from 'vitest'

import { vertex } from '../lib/algebraic-graphs/adjacency-map-directed-graph.js'
import { DirectedEdge } from '../lib/algebraic-graphs/edge.js'
import {
  emptyEdgeLabel,
  DefaultEdgeAttributes,
} from '$lib/layout-ir/ladder-graph/edge-attributes.js'
import { NumberWrapper } from './number-wrapper.js'

describe('Edge Attributes - DefaultEdgeAttributes', () => {
  test('DefaultEdgeAttributes have empty label by default', () => {
    const attrs = new DefaultEdgeAttributes()
    expect(attrs.getLabel()).toBe(emptyEdgeLabel)
  })

  test('DefaultEdgeAttributes can be created with a label', () => {
    const attrs = new DefaultEdgeAttributes('Test Label')
    expect(attrs.getLabel()).toBe('Test Label')
  })
})

describe('Edge Attributes - Setting and Getting', () => {
  test('Setting and getting edge attributes', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const g = vertex(nw1).connect(vertex(nw2))

    // Create some attributes
    const edgeAttrs = new DefaultEdgeAttributes('Test Edge')

    // Set attributes on the edge from nw1 to nw2
    const edge = new DirectedEdge(nw1, nw2)
    g.setEdgeAttributes(edge, edgeAttrs)

    // Get the attributes
    const retrievedAttrs = g.getAttributesForEdge(edge)
    expect(retrievedAttrs.getLabel()).toBe('Test Edge')
  })

  test('Setting edge attribute on non-existent edge throws error', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const nw3 = new NumberWrapper(3)

    const g = vertex(nw1).connect(vertex(nw2))

    const edgeAttrs = new DefaultEdgeAttributes()
    edgeAttrs.setLabel('Non-existent Edge')
    const edge = new DirectedEdge(nw1, nw3)

    expect(() => g.setEdgeAttributes(edge, edgeAttrs)).toThrowError(
      `setEdgeAttribute: Edge (${edge.u}, ${edge.v}) does not exist`
    )
  })
})

describe('Edge Attributes - Merging', () => {
  test('Merging edge attributes', () => {
    const attrs1 = new DefaultEdgeAttributes('Label1')
    const attrs2 = new DefaultEdgeAttributes('Label2')

    const mergedAttrs = attrs1.merge(attrs2)

    expect(mergedAttrs.getLabel()).toBe('Label2')
  })

  test('Merging edge attributes when one is empty', () => {
    const attrs1 = new DefaultEdgeAttributes('')
    const attrs2 = new DefaultEdgeAttributes('Label2')

    const mergedAttrs = attrs1.merge(attrs2)

    expect(mergedAttrs.getLabel()).toBe('Label2')
  })

  test('Merging edge attributes when second one is empty', () => {
    const attrs1 = new DefaultEdgeAttributes('Label1')
    const attrs2 = new DefaultEdgeAttributes()

    const mergedAttrs = attrs1.merge(attrs2)

    expect(mergedAttrs.getLabel()).toBe('Label1')
  })
})

describe('Edge Attributes - Graph Operations', () => {
  test('Edge attributes are preserved during overlay', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)

    const edge = new DirectedEdge(nw1, nw2)

    const g1 = vertex(nw1).connect(vertex(nw2))
    const attr1 = new DefaultEdgeAttributes('Label1')
    g1.setEdgeAttributes(edge, attr1)

    const g2 = vertex(nw1).connect(vertex(nw2))
    const attr2 = new DefaultEdgeAttributes('Label2')
    g2.setEdgeAttributes(edge, attr2)

    const overlaid = g1.overlay(g2)

    // Get edge attributes from overlaid graph
    const resultAttr = overlaid.getAttributesForEdge(edge)
    expect(resultAttr.getLabel()).toBe('Label2')
  })

  test('Non-colliding edge attributes are preserved by connect', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const nw3 = new NumberWrapper(3)

    const edge1 = new DirectedEdge(nw1, nw2)
    const edge2 = new DirectedEdge(nw1, nw3)

    const g1 = vertex(nw1).connect(vertex(nw2))
    const attr1 = new DefaultEdgeAttributes('Edge1')
    g1.setEdgeAttributes(edge1, attr1)

    const g2 = vertex(nw1).connect(vertex(nw3))
    const attr2 = new DefaultEdgeAttributes('Edge2')
    g2.setEdgeAttributes(edge2, attr2)

    // Connect g1 and g2
    const connected = g1.connect(g2)

    // Get edge attributes from connected graph
    const resultingAttr1 = connected.getAttributesForEdge(edge1)
    const resultingAttr2 = connected.getAttributesForEdge(edge2)

    expect(resultingAttr1.getLabel()).toBe('Edge1')
    expect(resultingAttr2.getLabel()).toBe('Edge2')
  })

  test('Edge attributes with overlapping edges during overlay are merged correctly', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)

    const edge = new DirectedEdge(nw1, nw2)

    const g1 = vertex(nw1).connect(vertex(nw2))
    const attr1 = new DefaultEdgeAttributes('Label1')
    g1.setEdgeAttributes(edge, attr1)

    // Create g2 with edge nw1 -> nw2, set attribute attr2 with empty label
    const g2 = vertex(nw1).connect(vertex(nw2))
    const attr2 = new DefaultEdgeAttributes('')
    g2.setEdgeAttributes(edge, attr2)

    const overlaid = g1.overlay(g2)

    const resultingAttr = overlaid.getAttributesForEdge(edge)
    expect(resultingAttr.getLabel()).toBe('Label1')
    // attr2's label is empty, so use attr1's label
  })
})
