# README

## Examples

Dec 18 2024: I've focused on examples that seemed trickier to me, though the examples I've highlighted also don't seem high-priority.

Examples that don't seem tricky but that would be worth using as examples in our cookbook / docs include, e.g.,:

### Matt Waddington's rewrite of part of the British Nationality Act (<https://osf.io/mt78r>):

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

#### Everything in good time

Let's break out the temporal aspects of this decision logic with temporal predicates.

1. A person P is a British citizen (at time T_0) if:
   a. the person was born (at time T_Birth)
      i. in the UK after commencement of this act (T_act)
      ii. in a qualifying territory on or after the appointed day (T_ad42)
   and
   b. AS OF T_Birth, some parent PP
      i. was a British citizen
      ii. was settled in the UK
      iii. (if a.ii then) was settled in a qualifying territory
      
      
Regarding 1.b.i, we accept _prima facie_ evidence, without having to derive the parent's own citizenship from first principles, particularly since we don't have, in scope, a way to decide citizenship before the time this act commenced (T_act) -- this is a bit like the soteriological problem of the fate of the unevangelized.

What is an appointed day?
The relevant day for the purposes of subsection (1A) or (3A) is the day appointed for the commencement of section 42 of the Borders, Citizenship and Immigration Act 2009 (which inserted those subsections).

* The other examples from `examples_for_parsing`

## Tech stack

The intention behind structured-examples.yaml is that it should be relatively easy to, e.g., render this in a webpage with functionality for filtering or sorting the examples. (See, e.g., Simon Willison's suite of datasette tools.)

But of course, there is always a tradeoff between how making the data more structured for future use and making it easier for a human to input it.
