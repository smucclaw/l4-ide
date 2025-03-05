# Committing an Offence separate from Liability for Penalty

> > Some criminal legislation/regulation is structured in an interesting way: instead of saying that a certain crime is punishable by a certain penalty, it will say that an offence is committed if X, Y, Z; multiple offences are defined; and then penalties are stated in a separate section. I suppose this makes sense because many offences can be committed together, and penalties can be assessed against them all at once.

> You're right about this interesting structure in criminal legislation. Many legal codes organize offenses and penalties separately for several practical reasons:

> * Efficiency in drafting - Defining offenses separately from penalties allows lawmakers to establish multiple related offenses without repetitively stating the same penalty provisions.
> * Graduated punishment schemes - This structure facilitates systems where penalties escalate based on factors like prior convictions or aggravating circumstances, without needing to redefine the underlying offense.
> * Judicial discretion - Separating offenses from penalties gives judges flexibility to consider multiple violations holistically when determining appropriate sentences.
> * Legislative updates - When legislatures wish to modify penalties (often due to changing social attitudes or prison capacity concerns), they can amend just the penalty provisions without redefining the substantive offenses.

> This approach is particularly common in regulatory frameworks where multiple technical violations might occur simultaneously. For example, environmental regulations might separately define various prohibited discharges, record-keeping violations, and permit infractions, while providing a unified penalty structure that accounts for factors like willfulness, harm caused, and economic benefit gained.

> Criminal codes often use this structure to establish a coherent sentencing philosophy across different offense categories, making the punishment system more consistent and proportional.

## Squeezing out the deontics

From the perspective of Gneezy & Rustichini's classic [A Fine Is A
Price](https://www.jstor.org/stable/10.1086/468061), we see that
penalties have at least two dimensions: economic/financial/monetary,
and moral/social.

What does "must" really mean?

When we tell a child "you must wash your hands" a born rebel will ask "or else what?"

If there is no consequence, is the "must" really a "must", or more of a "should"? Or a "may"?

These questions are endlessly debated by philosophers.

L4's position is set out below.

## The Object Level: from a coldly dispassionate perspective, an automaton simply executes a trace.

> When you borrow a book from a library, a clock starts ticking.

> You could return the book after a day.

> You could return the book after six months.

> One of those choices leads to a fine, and restricted borrowing privileges.

> One of those choices does not.

> If you pay the fine, the library will let you check out more books.

> If you do not pay the fine, the library will not.

The above wording may appear oddly non-judgmental.

Most library rules tend to use more weighted phrasing: "you **must** return borrowed books within 14 days, or be subject to a fine."

"You **may not** borrow books if a fine is outstanding."

"When no fines are outstanding, you **may** borrow books for up to 14 days, unless they are in the reserve."

These are "deontic modals". The "if" and "unless" keywords are "conditional operators".



## State

The immediate consequences of noncompliance are often spelled out in close proximity to the regulative rule that prescribes the required behaviour.

But sometimes the penalties are written in a different section.

"A person who commits an offence under sections 10 through 20 is liable to the following penalties, according to the number of offences committed ..."

The offences are first enumerated, the way a diner might speak to a
waiter, who writes down the order on a notepad; later, the penalties
are assessed, the way the bill is calculated at the end of the meal.

The notepad relays the order from the patron via the waiter to the bill.

In L4, the State mechanism relays information about offences to the penalty section.

## Object-Level versus Assertion-Level

L4 uses a state transition formalism to represent the moving parts of
a regulative rule. Any time somebody has to do something -- or refrain
from doing something -- within a certain time period, and face
consequences for noncompliance, we use L4's regulative statements to
express those rules.

That formalism *could* use the bloodless form of the rules: one thing
leads to another and another and another. We could render a finite
state automaton, or a state transition system, using the most
unopinionated, nonjudgemental language imaginable. This is the "object
level" representation of a normative system.

Maybe a library user doesn't care if they lose borrowing privileges
forever. Then the "you must pay a fine" is a toothless rule: it is not
strongly *enforceable*.

## Deontic Statements are Bounded

The statement "you must pay a fine" carries with it an unspoken
complement: "or else you will not be allowed to borrow again."

These complements usually exist, yet they usually go unspoken, in informal discourse.

When you hear a "must" but don't hear the explicit consequence, the
consequence is frequently "or you commit an offence, and are subject
to some sort of legal penalty."

Or it might be "or you sin against your fellows, and are subject to
social misapprobation."

"You must pick up your child from childcare before 7pm."

In L4, every deontic statement is bounded. Even a "may" permission is
bounded, in that taking that course of action that eventually causes
some other party to assume some obligation that they would otherwise
not.

## Deontic consequences are explicated at the Assertion Level

"You must do this thing if you don't want to pay a monetary penalty"

"There is no way to reach a FULFILLED outcome without paying a penalty, other than by doing this thing."

These kinds of statements are formalized in LTL / CTL as assertions,
or properties, *over* the object level of the state transition system.

## Is UPON sufficient or do we need a WHENCE?

L4's state transition model is oriented around the regulative clause, typically structured `PARTY p MUST Action BEFORE D HENCE c1 LEST c2`.

The `c1` and `c2` are themselves clauses, or combinations of clauses `c1a AND c1b`.

However we may want to dip into a State monad that allows us to update certain variables.

``` l4
§ 1 abuse of dogs
 GIVEN p IS A Person
       d IS A Dog
 EVERY p
 SHANT abuse d
  LEST `an offence is committed`
       PUT p.points = p.points + 3

§ 2 abuse of cats
 GIVEN p IS A Person
       c IS A Cat
 EVERY p
 SHANT abuse c
  LEST `an offence is committed`
       PUT p.points = p.points + 2

§ 9 penalties
 UPON `an offence is committed` `under`  § 1
                                         § 2
 PARTY p
  MUST  `go to jail` "for"
          SUM p.points
	    "years"
```
