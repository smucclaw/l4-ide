# User Input Pattern: the Defaultable Bool

How many ways can we architect a Boolean any/all tree?

## Running Example

> You may cross the border on foot or in a vehicle. If you're in a vehicle, you need to fill out the Vehicle Form.

> On foot includes wheelchairs, skateboards, and in-line skates.

> Vehicles include bicycles, motorcycles, two-door cars, four-door cars, hatchbacks, vans, buses, lorries, trucks, etc.

Let's pretend we're building an interactive web app which computes
certain "output" parts of the above rules -- "on foot", "in vehicle",
and "may cross the border".

"Do I need the Vehicle Form?" is tied to the "in vehicle" element.

We want the web app to automatically update with answers as soon as
the user clicks on the relevant input widgets.

## Exhaustive, Binary

The naive approach asks the end-user for every single leaf node, aka
ground term; undefined values are not allowed. Each ground term is
true or false.

```haskell
type ID = String
data BoolTree1 = Leaf Bool ID
               | Not  BoolTree1
	           | Any [BoolTree1]
	           | All [BoolTree1]
```

This approach would annoy the end-user quickly, because even after
saying they are in a motorcycle they would still have to say they are
not in a car, or van, or bus.

A form working with the data model would not pass input validation
until every single input is provided. We can't build a practical
live-updating evaluator of type `BoolTree1 -> Bool` around this data
structure, because the `BoolTree1` can't properly represent the start
state of the form, when the user hasn't clicked on anything yet.

## Ternary Logic

Every ground term can be true, false, or unknown. You can
short-circuit: `(True OR Unknown)` is `True`.

```haskell
type Ternary = Maybe Bool
data BoolTree2 = Leaf Ternary ID
               | Not  BoolTree2
	           | Any [BoolTree2]
	           | All [BoolTree2]
```

This is better. We could start the form with everything set to
`Nothing`. A live-updating evaluator (of type `BoolTree2 -> Bool`)
could consume those `Nothing`s, run to completion, and give an answer.
The moment the "motorcycle" node turns `Just True` the overall
computation evaluates to `True`.

See also https://en.wikipedia.org/wiki/Ternary_logic

## Negation as Failure

The evaluation engine may use "negation as failure" to collapse unknowns to false. `True AND Unknown` is `False`.

## With Defaults

Ground terms are equipped with default values. If the end-user chooses
not to explicitly set a term to true, false, or unknown, the default
value is used. The default value is one of true, false, or unknown.

Default values are used to support default reasoning: if we say "a
person" we assume the central case -- that it is a natural person,
with mental capacity.

In this example, we use it to suggest to the end-user that they're
probably in a vehicle, which is the common case, and they should just
accept the defaults and click through to the Vehicle Form.

```haskell
type WithDefault = Either Ternary Ternary
data BoolTree3 = Leaf WithDefault ID
               | Not  BoolTree2
	           | Any [BoolTree2]
	           | All [BoolTree2]
```

Of course we do still ask the user to confirm the assumption. But we
set the common-case default for usability. Here the `form` serves
multiple purposes:

- it records the overall structure of the Boolean logic constructed from the upstream L4;
- it reflects the assumption about the car, also given in the L4 with `TYPICALLY`, in the `Left` of `Either`;
- it records end-user input values in the `Right` of `Either`
- the whole thing is input to an evaluator

```haskell
form = Any [Any [ Leaf (Left Nothing) "On Actual Foot"
                , Leaf (Left Nothing) "Wheelchair"
                , Leaf (Left Nothing) "Skateboard"
                , Leaf (Left Nothing) "In-Line Skates" ]
	       ,Any [ Leaf (Left Nothing)     "Bicycle"
                , Leaf (Left Nothing)     "Motorcycle"
                , Leaf (Left Nothing)     "Two-Door Car"
                , Leaf (Left (Just True)) "Four-Door Car"
                , Leaf (Left Nothing)     "Hatchback"
				]
	       ]
```

When the end-user updates the form to say Actually I'm In A Motorcycle:

```haskell
                  Leaf (Right True)   "Motorcycle"
```

Then that user-given value outweighs the default assumption about the car.

## Parents Assignable

The end-user can set values not just for leaf nodes, but parent nodes also.

```haskell
type Label     = WithDefault ID
data BoolTree4 = Leaf WithDefault ID
               | Not  Label  BoolTree4
               | Any  Label [BoolTree4]
               | All  Label [BoolTree4]
```

An experienced end-user might say, "look, I'm in a vehicle, don't
worry your little head about the details, just give me the form."

So we would give an explicit name to the "in a vehicle" parent, and
allow the end-user to give it a value just like any other leaf.

```haskell
form = Any             (Left Nothing, "can cross border")
           [Any        (Left Nothing, "on foot")
                [ Leaf (Left Nothing, "On Actual Foot")
                , Leaf (Left Nothing, "Wheelchair"    )
                , Leaf (Left Nothing, "Skateboard"    )
                , Leaf (Left Nothing, "In-Line Skates") ]
           ,Any        (Left Nothing, "in a vehicle")
                [ Leaf (Left Nothing,     "Bicycle"       )
                , Leaf (Left Nothing,     "Motorcycle"    )
                , Leaf (Left Nothing,     "Two-Door Car"  )
                , Leaf (Left (Just True), "Four-Door Car" )
                , Leaf (Left Nothing,     "Hatchback"     )
                ]
	       ]
```

If the user doesn't know if a bicycle counts as a vehicle or not, they
can opt to expand the definition into a tree of checkboxed ground
terms, and use the computer to help think through that definition. But
if they are satisfied they know enough to just assign to the parent
node, they can go ahead and do that.

```haskell
	       ,Any  (Right (Just True), "in a vehicle")
```

Note that by default parent nodes usually have `Left Nothing`, even if
descendants have default values that would turn them true; we don't
want to pre-cache that result. We want to recompute each time, because
premature optimization is the root of all evil.

What happens if the parent assignment conflicts with the computed
value based on the children? Probably want to raise a warning at the
UI level, but allow it; this is the sort of thing that
humans-in-the-loop routinely want to do. To be super pedantic we can
run everything through a combinator that prepends `human_override ??`
to every node -- borrowing [that idea from JS](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Nullish_coalescing).

## Non-Boolean Inputs

We broaden the input widgets to allow non-Boolean values, as long as
they are things that convert very soon into Boolean.

For example we might want to know if someone is 21 years of age.

We could simply ask if they are "over 21 years of age". That would be
a `Boolean`, or rather our `WithDefault Ternary` taking the value `Right (Just True)`.

Or we could give them a text input box for "how many years old are you".

Or we could give them a date picker for "when was your birthday".

Or, and this is the most likely real-world scenario, the date picker
is just hidden part of some database lookup that operates against some
existing user account profile information; we don't have to ask the
user at all. But the evaluator still needs to know it to be able to
calculate the result.

## Composition with XOR

A close read may suggest that one crosses the border either on foot, or in a vehicle, but not both. So the logic is really an XOR:

```haskell
form = Xor             (Left Nothing, "can cross border")
           [Any        (Left Nothing, "on foot")      [...]
           ,Any        (Left Nothing, "in a vehicle") [...]
	       ]
```

This opens the door to other operators, like `AtLeast Int`.
