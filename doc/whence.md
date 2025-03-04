# Committing an Offence separate from Liability for Penalty

> > Some criminal legislation/regulation is structured in an interesting way: instead of saying that a certain crime is punishable by a certain penalty, it will say that an offence is committed if X, Y, Z; multiple offences are defined; and then penalties are stated in a separate section. I suppose this makes sense because many offences can be committed together, and penalties can be assessed against them all at once.

> You're right about this interesting structure in criminal legislation. Many legal codes organize offenses and penalties separately for several practical reasons:

> * Efficiency in drafting - Defining offenses separately from penalties allows lawmakers to establish multiple related offenses without repetitively stating the same penalty provisions.
> * Graduated punishment schemes - This structure facilitates systems where penalties escalate based on factors like prior convictions or aggravating circumstances, without needing to redefine the underlying offense.
> * Judicial discretion - Separating offenses from penalties gives judges flexibility to consider multiple violations holistically when determining appropriate sentences.
> * Legislative updates - When legislatures wish to modify penalties (often due to changing social attitudes or prison capacity concerns), they can amend just the penalty provisions without redefining the substantive offenses.

> This approach is particularly common in regulatory frameworks where multiple technical violations might occur simultaneously. For example, environmental regulations might separately define various prohibited discharges, record-keeping violations, and permit infractions, while providing a unified penalty structure that accounts for factors like willfulness, harm caused, and economic benefit gained.

> Criminal codes often use this structure to establish a coherent sentencing philosophy across different offense categories, making the punishment system more consistent and proportional.

## Is UPON sufficient or do we need a WHENCE?

L4's state transition model is oriented around the deontic clause, typically structured `PARTY p MUST Action BEFORE D HENCE c1 LEST c2`.

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
