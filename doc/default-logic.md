# Default Logic in L4

## Example 1: Parking

Generally speaking, legislation tends to put the common cases front and center, and deals with exceptions later.

There is nothing wrong with this: it mirrors the way people think.

"You must pay to park here. Unless it's a weekend. Or a public holiday. Or you have a long-term parking permit. Or you're an ambulance. Actively responding to a call. Or you're a foreign diplomat. Except in New York City. Because nobody messes with NYC DOT, not even an ambassador with diplomatic immunity."

## Decision Logic

In software, programmers formalize this pattern in the form of an "if/then/else" statement.

Here's some Python that defines a decision – whether one can park – given the elements on which the decision depends. Those elements form the "fact pattern". They are "inputs". We say the decision is a *function* of those inputs, just as `y` is a function of `x`, in the schoolboy's algebraic formula `y = m(x) + b`.

``` python
def can_park(day, public_holiday, has_permit, is_ambulance, is_diplomat, city):
    if   day in ['Saturday', 'Sunday']:            return "No need to pay"
    elif public_holiday:                           return "No need to pay"
    elif has_permit:                               return "No need to pay"
    elif is_ambulance and is_responding_to_call:   return "No need to pay"
    elif is_diplomat:
        if city == 'New York City':                return "Must pay to park"
        else:                                      return "No need to pay"
    else:                                          return "Must pay to park"
```

In the code block above, we pronounce `def` as "define", and we pronounce `elif` as "else if". We pronounce the `:` as "then". We `return` the output decision.

Two points worth noting.

Unlike in law, the default case goes at the bottom, and the special cases go at the top. But that is simply a matter of style. Humans like to see the general case early and the exceptions later. Computers like to consider the special cases first, and fall through to the general case only if none of the special cases apply. Many programmers read an "if/then/else" statement by eyeballing the "else" default first, and then going back to the top to read the "if" cases.

The "if/then/else" pattern can nest: the default case is that you must pay; but in the special case of a diplomat, the default within that special case is that you don't need to pay, unless (in the special case to the special case) you're in New York City. This sort of nesting can go many levels deep. Programmers use the special word "recursion" to talk about nesting within nesting within nesting.

We'll return to this example, writing it in L4, later.

## Example 2: British Nationality Act

The British Nationality Act decides a person's citizenship based on a whole host of factors, but the most common case is simply that a child born in the UK to British parents is also British.

The BNA sensibly puts that case front and center, leading with it right at the start, in §1.1 of the statute.

The BNA then proceeds to deal with rarer cases. For hundreds of pages.

What if a child is an orphan, left at the doorstep of a hospital or a fire station? The father and mother might not be known to the system. The BNA deals with that in §1.2.

What if the parents are known to the system, but are not citizens, and give the child up for adoption, and the child is adopted by British parents? The BNA deals with that in §1.5.

What about children born outside the UK, to British citizens, in some far-flung outpost of what used to be part of the Empire? The BNA deals with that in §2.

The "if/then/else" pattern is sufficient to capture all of that complexity:

``` python
def citizenship(born_in_uk, parents_british, is_orphan, adopted_by_british):
    if born_in_uk and parents_british:        return "British"
    elif is_orphan:                           return "British"
    elif adopted_by_british:                  return "British"
    elif not born_in_uk and parents_british:  return "British"
    else:                                     return "Not British"
```

This logic makes it clear that by default, any person is not a British citizen, unless certain criteria are met. As most people on the planet are not British, that is a reasonable default. Each line in this program deserves a separate paragraph or more in the BNA, to explain the details of each requirement -- age of child, residency requirements, which overseas territory qualify, that sort of thing. The above is only a broad sketch to illustrate the logic.

This example lets us focus on an important related idea: the notion of optional data types.

We have already seen defaults in action, as the "else" part of an "if/then/else" statement. Applying the rules to a set of facts, we short-circuit early if a special-case matches; and if none of the special cases match, we return the default in the "else".

Most of the time, the special cases don't apply, even though there are a great many of them, and we proceed to the common, general, default case.

(If, most of the time, a special case did apply, then it wouldn't be a special case any more: it would be the common case!)

It's important that an "if/then/else" rule specify the default case. If rules only talked about special cases and never addressed the defaults, the world would be thrown into chaos.

Let's avoid chaos.

## Sometimes, facts are unavailable.

A version of this idea sometimes shows up in the facts themselves.

Suppose a rule asks the question: "is the father British?"

In the common case, we know who a baby's parents are; we know their names; we know if they are British. We can answer the question.

In the uncommon case, a baby is discovered, anonymous and orphaned, on the doorstep of a convent, or in a basket on the Nile. The question goes unanswered. Decades later, the baby ends up surprising everybody.

There is potential for chaos here.

Rigour demands that every rule that deals with a child's parents, must also consider the special-case possibilities that a child's father or mother may not be known! A child may not have a father! The answer to "is the father British?" is not just "yes" or "no" -- it could be "we don't know."

Programmers recognize "we don't know" as an important problem in the domain of data modelling, and strive to tackle it seriously.

When filling out a form that asks for the father's name, we might leave that part of the form blank, or write "N/A".

When doing the data entry for that form, what do we do when the father's name is written as "Not Applicable"? Or "Null"? It wouldn't make sense to just type in "Not Applicable", or "Null", because that would lead to a toddler telling people "my daddy's name is Null", instead of the more delicate "My mother is single, and my father is not in the picture."

Every computer record for "father's details" now needs to make room for an adjacent field: "no paternal details". If that field is true, then "father's details" does not apply. If the field is false, then we know it is safe to ask for the father's name and citizenship.

## Tony Hoare's "Billion-Dollar Mistake"

When computing was young, bytes were expensive, and data modelling was more an art than a science, programmers opted to represent nulls directly within the fields: similar to saying, "if we don't know what value a number is, let's pretend it's zero"; or "if we don't know what the father's name is, let's pretend it's an empty string."

This turned out to be a mistake. Tony Hoare, inventor of the null, later described this as his billion-dollar mistake, because it led to the software equivalent of toddlers going around telling people their father's name was Not Applicable.

## How Modern Languages Deal with Nulls

More recently, programming languages have introduced the convention of an Optional type. We've already talked about numbers and strings: these are simple types. As data modellers, we are empowered to conceive of new types. Say, colour. We might describe things as usually having a colour: an apple could be red or green, a car could be white or black. But what colour is a dream? What colour is a mirror? "Not applicable". So when we attribute colours to things, instead of confidently but wrongly saying Every Thing Has A Colour, we more cautiously say Every Thing Might Have A Colour; or, equivalently, Every Thing Has An Optional Colour Attribute. If the thing isn't colourable, we say its colour attribute is Nothing, or Empty, or None, or Undefined. If the thing is colourable, we say its colour attribute is Present, or Defined, or Some value, or Just whatever.

L4's Optional type, or Maybe type, works the same way: 

## In most scenarios, most terms are Optional

A naive data model might say:

```
  DECLARE Person
    HAS name      IS A String
        birthdate IS A Date
        father    IS A Person
        mother    IS A Person
        id        IS A Number
```

But many countries allow baby paperwork to be filed without a definite name. Some states allow parents to write down "Baby Boy" or "Baby Girl" and amend the name later. That's the equivalent of a null value.

A more sophisticated data model might say:

```
  DECLARE Person
    HAS name      IS AN Optional String
        birthdate IS A  Date
        father    IS AN Optional Person
        mother    IS AN Optional Person
        id        IS AN Optional Number
```

This gives us the flexibility to recognize a more complex reality: that sometimes there is no father or mother, sometimes the name hasn't been assigned, sometimes the ID number hasn't been issued by the state.

Here we've left left `birthdate` as a non-Optional. Does this make sense? Or can you think of a scenario -- an "edge case" -- where it would be better off as Optional field too?

## This is also called the Maybe Monad

Sometimes L4 documentation will refer to `Optional` attributes as `Maybe` attributes. That means the same thing, it is just a different jargon convention.


