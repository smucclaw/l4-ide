import { describe, test, expect } from 'vitest'

import { vertex } from '../lib/algebraic-graphs/adjacency-map-directed-graph.js'
import {
  emptyEdgeLabel,
  DefaultEdgeAttributes,
  DirectedEdge,
  EmptyEdgeStyles,
  HighlightedEdgeStyles,
} from '../lib/algebraic-graphs/edge.js'
import { NumberWrapper } from './number-wrapper.js'

describe('Edge Attributes - DefaultEdgeAttributes', () => {
  test('DefaultEdgeAttributes have fallback / default styles and empty label', () => {
    const attrs = new DefaultEdgeAttributes()
    expect(attrs.getStyles().getStrokeColor()).toBe(
      '--default-internal-stroke-color'
    )
    expect(attrs.getLabel()).toBe(emptyEdgeLabel)
  })
})

describe('Edge Attributes - Setting and Getting', () => {
  test('Setting and getting edge attributes', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const g = vertex(nw1).connect(vertex(nw2))

    // Create some attributes
    const edgeAttrs = new DefaultEdgeAttributes(
      new HighlightedEdgeStyles(),
      'Test Edge'
    )

    // Set attributes on the edge from nw1 to nw2
    const edge = new DirectedEdge(nw1, nw2)
    g.setEdgeAttributes(edge, edgeAttrs)

    // Get the attributes
    const retrievedAttrs = g.getAttributesForEdge(edge)
    expect(retrievedAttrs.getLabel()).toBe('Test Edge')
    expect(retrievedAttrs.getStyles().getStrokeColor()).toBe(
      new HighlightedEdgeStyles().getStrokeColor()
    )
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
    const styles1 = new EmptyEdgeStyles()
    const styles2 = new HighlightedEdgeStyles()

    const attrs1 = new DefaultEdgeAttributes(styles1, 'Label1')
    const attrs2 = new DefaultEdgeAttributes(styles2, 'Label2')

    const mergedAttrs = attrs1.merge(attrs2)

    expect(mergedAttrs.getLabel()).toBe('Label2')
    expect(mergedAttrs.getStyles().getStrokeColor()).toBe(
      styles2.getStrokeColor()
    )
  })

  test('Merging edge attributes when one is empty', () => {
    const styles1 = new EmptyEdgeStyles()
    const styles2 = new HighlightedEdgeStyles()

    const attrs1 = new DefaultEdgeAttributes(styles1, '')
    const attrs2 = new DefaultEdgeAttributes(styles2, 'Label2')

    const mergedAttrs = attrs1.merge(attrs2)

    expect(mergedAttrs.getLabel()).toBe('Label2')
    expect(mergedAttrs.getStyles().getStrokeColor()).toBe(
      styles2.getStrokeColor()
    )
  })
})

describe('Edge Attributes - Graph Operations', () => {
  test('Edge attributes are preserved during overlay', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)

    const edge = new DirectedEdge(nw1, nw2)

    const g1 = vertex(nw1).connect(vertex(nw2))
    const attr1 = new DefaultEdgeAttributes()
    attr1.setLabel('Label1')
    g1.setEdgeAttributes(edge, attr1)

    const g2 = vertex(nw1).connect(vertex(nw2))
    const attr2 = new DefaultEdgeAttributes(
      new HighlightedEdgeStyles(),
      'Label2'
    )
    g2.setEdgeAttributes(edge, attr2)

    const overlaid = g1.overlay(g2)

    // Get edge attributes from overlaid graph
    const resultAttr = overlaid.getAttributesForEdge(edge)
    expect(resultAttr.getLabel()).toBe('Label2')
    expect(resultAttr.getStyles().getStrokeColor()).toBe(
      attr2.getStyles().getStrokeColor()
    )
  })

  test('Non-colliding edge attributes are preserved by connect', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const nw3 = new NumberWrapper(3)

    const edge1 = new DirectedEdge(nw1, nw2)
    const edge2 = new DirectedEdge(nw1, nw3)

    const g1 = vertex(nw1).connect(vertex(nw2))
    const attr1 = new DefaultEdgeAttributes()
    attr1.setLabel('Edge1')
    g1.setEdgeAttributes(edge1, attr1)

    const g2 = vertex(nw1).connect(vertex(nw3))
    const attr2 = new DefaultEdgeAttributes()
    attr2.setLabel('Edge2')
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
    const attr1 = new DefaultEdgeAttributes(new EmptyEdgeStyles(), 'Label1')
    g1.setEdgeAttributes(edge, attr1)

    // Create g2 with edge nw1 -> nw2, set attribute attr2
    const g2 = vertex(nw1).connect(vertex(nw2))
    const attr2 = new DefaultEdgeAttributes(new HighlightedEdgeStyles())
    g2.setEdgeAttributes(edge, attr2)

    const overlaid = g1.overlay(g2)

    const resultingAttr = overlaid.getAttributesForEdge(edge)
    expect(resultingAttr.getLabel()).toBe('Label1')
    // attr2's label is empty, so use attr1's label
    expect(resultingAttr.getStyles()).toBe(attr2.getStyles())
    // Styles should be from attr2
  })
})
