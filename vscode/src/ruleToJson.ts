export type AndOrTag = 'All' | 'Any' | 'Leaf';
export type ShouldView = 'Ask' | 'View';
export type MarkSource = 'user';

export interface AndOrNode {
  tag: AndOrTag;
  children?: RuleNode[];
  contents?: string;
}

export interface RuleNode {
  andOr: AndOrNode;
  mark: {
    value: 'undefined';
    source: MarkSource;
  };
  prePost: Record<string, string>;
  shouldView: ShouldView;
}
