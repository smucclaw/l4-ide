# README

## Examples

Dec 18 2024: I've focused on examples that seemed trickier to me, though the examples I've highlighted also don't seem high-priority.

Examples that don't seem tricky but that would be worth using as examples in our cookbook / docs include, e.g.,:

* Matt Waddington's rewrite of part of the British Nationality Act (<https://osf.io/mt78r>):

```text
1(1) A person is a British citizen if –
  (a) the person is born –
      (i) in the United Kingdom after commencement, or
      (ii) in a qualifying territory on or after the appointed day; and
  (b) when the person is born, the person’s father or mother is –
      (i) a British citizen;
      (ii) settled in the United Kingdom; or
      (ii) settled in the qualifying territory in which the person is born.
```

* The other examples from `examples_for_parsing`

## Tech stack

The intention behind structured-examples.yaml is that it should be relatively easy to, e.g., render this in a webpage with functionality for filtering or sorting the examples. (See, e.g., Simon Willison's suite of datasette tools.)

But of course, there is always a tradeoff between how making the data more structured for future use and making it easier for a human to input it.
