import { describe, test, expect } from 'vitest'

import { vertex } from '../lib/algebraic-graphs/adjacency-map-directed-graph.js'
import {
  DefaultEdgeAttributes,
  DirectedEdge,
  EmptyEdgeStyles,
  SelectedEdgeStyles,
} from '../lib/algebraic-graphs/edge.js'
import { NumberWrapper } from './number-wrapper.js'

describe('Edge Attributes - DefaultEdgeAttributes', () => {
  test('DefaultEdgeAttributes have empty styles and empty label', () => {
    const attrs = new DefaultEdgeAttributes()
    expect(attrs.getStyles().getRawStyles()).toBe('')
    expect(attrs.getLabel()).toBe('')
  })
})

describe('Edge Attributes - Setting and Getting', () => {
  test('Setting and getting edge attributes', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const g = vertex(nw1).connect(vertex(nw2))

    // Create some attributes
    const edgeAttrs = new DefaultEdgeAttributes(
      new SelectedEdgeStyles(),
      'Test Edge'
    )

    // Set attributes on the edge from nw1 to nw2
    const edge = new DirectedEdge(nw1, nw2)
    g.setEdgeAttribute(edge, edgeAttrs)

    // Get the attributes
    const retrievedAttrs = g.getAttributesForEdge(edge)
    expect(retrievedAttrs.getLabel()).toBe('Test Edge')
    expect(retrievedAttrs.getStyles().getRawStyles()).toBe(
      new SelectedEdgeStyles().getRawStyles()
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

    expect(() => g.setEdgeAttribute(edge, edgeAttrs)).toThrowError(
      `setEdgeAttribute: Edge (${edge.u}, ${edge.v}) does not exist`
    )
  })
})

describe('Edge Attributes - Merging', () => {
  test('Merging edge attributes', () => {
    const styles1 = new EmptyEdgeStyles()
    const styles2 = new SelectedEdgeStyles()

    const attrs1 = new DefaultEdgeAttributes(styles1, 'Label1')
    const attrs2 = new DefaultEdgeAttributes(styles2, 'Label2')

    const mergedAttrs = attrs1.merge(attrs2)

    expect(mergedAttrs.getLabel()).toBe('Label2')
    expect(mergedAttrs.getStyles().getRawStyles()).toBe(styles2.getRawStyles())
  })

  test('Merging edge attributes when one is empty', () => {
    const styles1 = new EmptyEdgeStyles()
    const styles2 = new SelectedEdgeStyles()

    const attrs1 = new DefaultEdgeAttributes(styles1, '')
    const attrs2 = new DefaultEdgeAttributes(styles2, 'Label2')

    const mergedAttrs = attrs1.merge(attrs2)

    expect(mergedAttrs.getLabel()).toBe('Label2')
    expect(mergedAttrs.getStyles().getRawStyles()).toBe(styles2.getRawStyles())
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
    g1.setEdgeAttribute(edge, attr1)

    const g2 = vertex(nw1).connect(vertex(nw2))
    const attr2 = new DefaultEdgeAttributes(new SelectedEdgeStyles(), 'Label2')
    g2.setEdgeAttribute(edge, attr2)

    const overlaid = g1.overlay(g2)

    // Get edge attributes from overlaid graph
    const resultAttr = overlaid.getAttributesForEdge(edge)
    expect(resultAttr.getLabel()).toBe('Label2')
    expect(resultAttr.getStyles().getRawStyles()).toBe(
      attr2.getStyles().getRawStyles()
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
    g1.setEdgeAttribute(edge1, attr1)

    const g2 = vertex(nw1).connect(vertex(nw3))
    const attr2 = new DefaultEdgeAttributes()
    attr2.setLabel('Edge2')
    g2.setEdgeAttribute(edge2, attr2)

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
    g1.setEdgeAttribute(edge, attr1)

    // Create g2 with edge nw1 -> nw2, set attribute attr2
    const g2 = vertex(nw1).connect(vertex(nw2))
    const attr2 = new DefaultEdgeAttributes(new SelectedEdgeStyles())
    g2.setEdgeAttribute(edge, attr2)

    const overlaid = g1.overlay(g2)

    const resultingAttr = overlaid.getAttributesForEdge(edge)
    expect(resultingAttr.getLabel()).toBe('Label1') // attr2 label is empty, so should keep attr1's label
    expect(resultingAttr.getStyles()).toBe(attr2.getStyles()) // Styles should be from attr2
  })
})
