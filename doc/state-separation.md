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

``` haskell
type ID = String
data BoolTree1 = Leaf Bool ID
               | Not  BoolTree1
	           | Any [BoolTree1]
	           | All [BoolTree1]
```

This approach would annoy the end-user quickly, because even after
saying they are in a motorcycle they would still have to say they are
not in a car, or van, or bus.

A form given the above data model would not pass input validation. We
can't build a practical live-updating evaluator of type `BoolTree1 ->
Bool` around this data structure.

## Ternary Logic

Every ground term can be true, false, or unknown. Unknown values are
allowed. `(True OR Unknown)` is `True`.

``` haskell
type Ternary = Maybe Bool
data BoolTree2 = Leaf Ternary ID
               | Not  BoolTree2
	           | Any [BoolTree2]
	           | All [BoolTree2]
```

This is better. Even with everything set to `Nothing`, a live-updating
evaluator (of type `BoolTree2 -> Bool`) could still run to
completion and give an answer. The moment the "motorcycle" node turns
`Just True` the overall computation evaluates to `True`.

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

``` haskell
type WithDefault = Either Ternary Ternary
data BoolTree3 = Leaf WithDefault ID
               | Not  BoolTree2
	           | Any [BoolTree2]
	           | All [BoolTree2]
```

This lets us assume, unless the user specifies otherwise, that they
are in a four-door vehicle. Of course we do still ask the user to
confirm the assumption. But we set the common-case default for
usability. Here the `form` serves multiple purposes:
- it records the overall structure of the Boolean logic constructed from the upstream L4;
- it reflects the assumption about the car;
- it records end-user input values in the `Right` of `Either`
- it is an input to an evaluator

``` haskell
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

``` haskell
                  Leaf (Right True)   "Motorcycle"
```

Then that user-given value outweighs the default assumption about the car.

## Parents Assignable

The end-user can set values not just for leaf nodes, but parent nodes also.

``` haskell
type Label     = WithDefault ID
data BoolTree4 = Leaf WithDefault ID
               | Not  Label  BoolTree4
               | Any  Label [BoolTree4]
               | All  Label [BoolTree4]
```

An experienced end-user might say, "look, I'm in a vehicle, don't
worry your little head about the details, just give me the form."

So there would be a checkbox against "in a vehicle".

``` haskell
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

``` haskell
	       ,Any  (Right (Just True), "in a vehicle")
```

Note that by default parent nodes usually have `Left Nothing`, even if
descendants have default values that would turn them true; we don't
want to pre-cache that result. We want to recompute each time, because
premature optimization is the root of all evil.

## Non-Boolean Inputs

We broaden the input widgets to allow non-Boolean values, as long as
they are things that convert very soon into Boolean.

For example we might want to know if someone is 21 years of age.

We could simply ask if they are "over 21 years of age". That would be
a `Boolean`, or rather our `WithDefault Ternary`.

Or we could give them a text input box for "how many years old are you".

Or we could give them a date picker for "when was your birthday".

Or, and this is the most likely real-world scenario, the date picker
is just hidden part of some database lookup that operates against some
existing user account profile information; we don't have to ask the
user at all. But the evaluator still needs to know it to be able to
calculate the result.


## Composition with XOR

A close read may suggest that one crosses the border either on foot, or in a vehicle, but not both. So the logic is really an

``` haskell
form = Xor             (Left Nothing, "can cross border")
           [Any        (Left Nothing, "on foot")      [...]
           ,Any        (Left Nothing, "in a vehicle") [...]
	       ]
```

This opens the door to other operators, like `AtLeast Int`.

