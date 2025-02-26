# Default Logic in L4

Rules can be structured to have a common case leading to a general answer; and zero or more special cases that lead to different answers.

The real world being what it is, decision-makers often lack sufficient information to be sure that a special case does hold; they give a provisional answer, caveated with assumptions.

Philosophers work with this pattern under the name "default logic".

As part of this pattern, it's important to have a way to represent the absence of information, as well as the presence of information.

## Example 1: Can Birds Fly?

The classic example goes something like this.

"Guess what? I have an imaginary friend with me. He is a bird. His name is Oswald. You can't see him, though."

"Hello, Oswald."

"Do you think Oswald can fly?"

"I would guess he can fly, because birds can fly."

"Ah, but that's where you're wrong! Oswald is a kakapo. A large flightless parrot."

"I see. Most birds can fly, but some birds can't. I had assumed that Oswald was not a penguin, or a kiwi, or an ostrich, emu, or cassowary. Or a kakapo, for that matter. Silly me."

"Why did you assume that?"

"Because there are over fifty billion birds in the world, and fewer than five hundred kakapo in the wild. As the saying goes, when you hear hoofbeats, think horses, not zebras; when you told me Oswald was a bird, I thought pigeon, not parrot."

"But Oswald *can* fly, after all!"

"Oh?"

"I just bought him a plane ticket to send him back to New Zealand!"

"What a privilege to have met Oswald, the ultimate edge case."

You can explore this sort of ontological tomfoolery further at [No Vehicles In The Park](https://www.novehiclesinthepark.com/).

## Example 2: Parking

Generally speaking, legislation tends to put the common cases front and center, and deals with exceptions later.

There is nothing wrong with this: it mirrors the way people think.

"You must pay to park here. Unless it's a weekend. Or a public holiday. Or you have a long-term parking permit. Or you're an ambulance. Actively responding to a call. Or you're a foreign diplomat. Except in New York City. Because nobody messes with NYC DOT, not even an ambassador with diplomatic immunity."

## The "If/Then/Else" Pattern

In software, programmers often formalize this sort of rule with an "if/then/else" statement.

Here's some Python that defines a decision -- whether one can park -- given the elements on which the decision depends. Those elements form the "fact pattern". They are "inputs". We say the decision is a *function* of those inputs, just as `y` is a function of `x`, in the algebraic formula `y = m(x) + b`.

``` python
def can_park(day, public_holiday, has_permit, is_ambulance, is_diplomat, city):
    if   day in ['Saturday', 'Sunday']:            return "No need to pay"
    elif public_holiday:                           return "No need to pay"
    elif has_permit:                               return "No need to pay"
    elif is_ambulance and is_responding_to_call:   return "No need to pay"
    elif is_diplomat:
      if city == 'New York City':                  return "Must pay to park"
      else:                                        return "No need to pay"
    else:                                          return "Must pay to park"
```

In the code block above, we pronounce `def` as "define", and we pronounce `elif` as "else if". We pronounce the `:` as "then". We `return` the output decision.

Two points worth noting.

Unlike in legal drafting, the default case goes at the bottom, and the special cases go at the top. But that is simply a matter of style. Humans like to see the general case early and the exceptions later. Computers like to consider the special cases first, and fall through to the general case only if none of the special cases apply. Many programmers read an "if/then/else" statement by eyeballing the "else" default first, and then going back to the top to read the "if" cases.

The "if/then/else" pattern can nest: the default case is that you must pay; but in the special case of a diplomat, the default within that special case is that you don't need to pay, unless (in the exception to the exception) you're in New York City. This sort of nesting can go many levels deep. It's indicated by indentation. Programmers use the special word "recursion" to talk about nesting within nesting within nesting.

We'll return to this example, writing it in L4, later.

## Example 2: British Nationality Act

The British Nationality Act decides a person's citizenship based on a whole host of factors, but the most common case is simply that a child born in the UK to British parents is also British.

The BNA sensibly puts that case front and center, leading with it right at the start, in §1.1 of the statute.

The BNA then proceeds to deal with rarer cases. For hundreds of pages.

What if a child is an orphan, left at the doorstep of a hospital or a fire station? The father and mother might be unknown. The BNA deals with that in §1.2.

What if the biological parents are known, but are not citizens, and give the child up for adoption, and the child is adopted by a new set of British parents? The BNA deals with that in §1.5.

What about children born outside the UK, to British citizens, in some far-flung outpost of what used to be part of the Empire? The BNA deals with that in §2.

The "if/then/else" pattern is sufficient to capture all of that complexity:

``` python
def citizenship(birthplace, parents_british, is_orphan, adopted_by_british):
    if birthplace == "UK" and parents_british:      return "British"
    elif is_orphan:                                 return "British"
    elif adopted_by_british:                        return "British"
    elif is_overseas_territory(birthplace) and parents_british:
      if birthplace == "Akrotiri and Dhekelia":     return "Not British"
      else:                                         return "British"
    else:                                           return "Not British"
```

This logic makes it clear that by default, any person is not a British citizen, unless certain criteria are met. As most people on the planet are not British, that is a reasonable default. Each line in this program deserves a separate paragraph or more in the BNA, to explain the details of each requirement -- age of child, residency requirements, which overseas territory qualify, that sort of thing. The above is only a sketch, to illustrate the logic.

# Optional Types in L4

We usually know important details, but sometimes we don't. We must always be prepared to handle both cases.

The BNA example introduces an important related idea: the notion of optional data types.

We have already seen defaults in action, as the "else" part of an "if/then/else" statement. Applying the rules to a set of facts, we short-circuit early if a special-case matches; and if none of the special cases match, we return the default in the "else".

Most of the time, the special cases don't apply, even though there are a great many of them, and we proceed to the common, general, default case.

(If, most of the time, a special case did apply, then it wouldn't be a special case any more: it would be the common case!)

It's important that an "if/then/else" rule should always specify the default case. If rules only talked about special cases and never addressed the defaults, the world would be thrown into chaos.

Let's avoid chaos.

What if we don't have enough information to know if the special case applies? Usually, we assume that it doesn't. A "rebuttable presumption" is that Oswald is an ordinary sort of bird, until we are informed it is a flightless bird; at that point, we revise our decision. We assume that most days aren't public holidays, and so, during the week, we usually go to pay for parking. In an adversarial scenario we have to talk about whether the burden of proof falls on one party or the other; but for the purposes of setting out the rules, let's just focus on the data modelling.

## The Case of the Missing Parent

Suppose a rule asks the question: "is the father British?"

In the common case, we know who the father is; we know if he is British. We can answer the question.

In the uncommon case, a baby is discovered, anonymous and orphaned, on the doorstep of a convent, or in a basket on the Nile. We don't know if the father is British because we don't know who the father is.

There is potential for chaos here. Especially where computers are concerned.

When filling out a paper form that asks for the father's name, we might leave that part of the form blank, or write "N/A".

If the form asks for the father's birthdate, we might just draw a line through the box.

When doing the data entry for that form, what do we do when the father's name is written as "Not Applicable"? Or "Null"? It wouldn't make sense to just type in "Not Applicable", or "Null", because that would lead to a toddler telling people "my daddy's name is Null", instead of the more delicate, and more accurate, "My mother raises me, and my father is not in the picture."

How do we record the birthdate? Maybe the form insists on a date. We could put in January 1 1900, as a sort of special-case placeholder. But as far as placeholders go, why not the 9th of September 1999? That is the sort of thing that led to the Y2K crisis. Better not to put in a bogus date at all. The form shouldn't insist on it.

The correct way to solve this problem is to make room, adjacent to "father's name" and "father's birthdate", for a new field: "no paternal details". If that field is set to true, then we know not to even consider the name and birthdate. If the field is false, then we know it is safe to go on.

This allows us to avoid the chaos. With order. With rigour.

Rigour demands that every rule that deals with a child's parents must also consider the special-case possibility that a child might not have parents! A child may not have a father on record! Or a mother! The answer to "is the father British?" is not just "yes" or "no" -- it could be "we don't know."

Good programmers recognize "we don't know" as an valid answer. The generally accepted solution is to always separate "what is the value of the thing?" from "do we even know the thing?"

The name of this solution is "the Maybe monad" if you're one sort of programmer, and "Optional types" if you're another sort.

## Tony Hoare's "Billion-Dollar Mistake"

When computing was young, bytes were expensive, and data modelling was more an art than a science. Programmers opted to represent nulls directly within the fields: similar to saying, "if we don't know what value a number is, let's pretend it's zero"; or "if we don't know what the father's name is, let's pretend it's an empty string."

This turned out to be a mistake. Tony Hoare, inventor of the null, later described this as his billion-dollar mistake, because it led to the software equivalent of toddlers going around telling people their father's name is Not Applicable, and that their father is over two thousand years old.

## Modern Languages Deal with Nulls using the Optional Type

This chaos led to the development of the Optional type in modern programming languages.

Quick example. We've already talked about numbers and strings: these are simple types. As data modellers, we are empowered to conceive new types. Say, colour. We might describe things as usually having a colour: an apple could be red or green, a car could be white or black.

But what colour is a dream? What colour is a mirror? "Not applicable".

So when we attribute colours to things, instead of confidently but wrongly saying Every Thing Has A Colour, we more cautiously say Every Thing Might Have A Colour; or, equivalently, Every Thing Has An Optional Colour Attribute. If the thing isn't colourable, we say its colour attribute is Nothing, or Empty, or None, or Undefined. If the thing is colourable, we say its colour attribute is Present, or Defined, or Some value, or Just whatever. Then we fill in the whatever value.

L4's Optional type, or Maybe type, works the same way.

## In the real world, many attributes are Optional

A naive data model might say:

```
  DECLARE Person
    HAS first_name  IS A String
        middle_name IS A String
        last_name   IS A String
        birthdate   IS A Date
        father      IS A Person
        mother      IS A Person
        id          IS A Number
```

But many countries allow baby paperwork to be filed without a definite name. Some states allow parents to write down "Baby Boy" or "Baby Girl" and amend the name later. That's the equivalent of a null value.

Many countries don't even distinguish middle names. Some countries are perfectly okay with just a single name: do we consider that a first or a last name? See <https://www.kalzumeus.com/2010/06/17/falsehoods-programmers-believe-about-names/> for more on that.

A more sophisticated data model might say:

```
  DECLARE Person
    HAS name      IS AN Optional String
        birthdate IS A  Date
        father    IS AN Optional Person
        mother    IS AN Optional Person
        id        IS AN Optional Number
```

This collapses "name" to a single string, and makes it optional, at that.

Now we have the flexibility to recognize a more complex reality: that sometimes there is no father or mother, sometimes the name hasn't been assigned, sometimes the ID number hasn't been issued by the state.

Here we've kept `birthdate` as a non-Optional. Does that make sense? Or can you think of a scenario -- an "edge case" -- where it would be better off as Optional field too?

## How does this work for the parking example?

A parking warden might see a Rolls Royce parked without paying. It's not clear whether the car belongs to a foreign embassy. So, write the ticket, and the embassy can pay -- or let the tab run.

A parking warden might see an ambulance parked without paying. Are they actively responding to a call? Nobody's inside the ambulance -- it's hard to say. So, write the ticket, and let the ambulance subsequently submit evidence that they were working. When the wheels of justice grind fine, sometimes they're grinding away at `Unknowns`, trying to sift them into `True` or `False`.

Operationally, the input to the decision function changes: the `is_responding_to_call` input starts off as `Unknown`. What does the parking warden do with `Unknown` values? Treat them, provisionally, as false, and write the ticket. Treating unknowns as false -- collapsing a ternary logic to a binary logic -- is called, by logicians, "negation as failure", as in "failure to prove that something is true, therefore considering it to be a negative, not a positive." We see "negation as failure" in law all the time: presumed innocent proven guilty is the most familiar example.

## The Optional Type is also called the Maybe Monad

While most programming languages call it the `Optional` type, Haskell prefers to call it a `Maybe` type.

The word "optional" suggests some sort of choice, but often when an attribute it's missing it's not like somebody affirmatively elected to not have that attribute.

No infant opts to have a missing parent.

Less value-laden (ha, ha) to just say that maybe we know it, and maybe we don't.

L4 documentation refers interchangeably to `Optional` and `Maybe` attributes. That means the same thing, it is just a different jargon convention.


# Further Reading

- 1980: R. Reiter, A Logic for Default Reasoning, 13 ARTIFICIAL INTELLIGENCE 81
- 2017: Lawsky, A Logic for Statutes. https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3088206
- 2020: http://catala-lang.org/
