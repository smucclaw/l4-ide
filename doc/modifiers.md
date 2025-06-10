# Modifiers for defeasibility

Certain modifiers frequently appear at the start of clauses.

Informally this subject is dealt with under the name "defeasibility", or "meta-rules".

## Principles

We use function composition to model "meta-rule" interactions between ordinary rules.

# Despite / Notwithstanding

## Examples

> §10 Beer taxes. The retail sale of beer shall be taxed at 6%.

> §11 Sunday beer. Notwithstanding §10, on Sundays, the retail sale of beer shall be taxed at 9%.

We consider these modifiers to indicate a priority relation that
resolves a conflict between two clauses by establishing a partial
ordering. They can also be understood as an exception vs default: the
exception has higher priority.

Usually, the ordering between clauses (functions) is limited to a
subset of possibilities (the domain). If the override were total then
what would be the point of giving the default?

# Subject To

Sometimes as simple as a priority relation, indicating exceptions to a default -- the dual to "despite / notwithstanding".

More comprehensively, a monadic function that transforms the inputs and outputs of one function by another.

See this discussion with Claude: https://claude.ai/share/d0c10a0d-9402-4bbc-bd83-0b1c0e5db42a

> §20 Wine taxes. Subject to §21, the retail sale of wine shall be taxed at 12%.

> §21 Sunday wine. Subject to §22, on Sundays, the retail tax rate of wine shall be increased by one-half of the existing rate. For example, a 10% rate shall increase to 15%.

> §22 Public Holidays. On public holidays, the retail sale of wine shall be untaxed.

The eagle-eyed will spot a conflict between clauses: what about a public holiday that does not fall on a Sunday?

Should we consider this a drafting error? To be remedied with a "Subject to §21 and §22"?

Or should we invoke the interpretation rules:

- Lex posterior derogat priori
- Lex specialis derogat legi generali

Our monadic "subject to" operator could also modify the inputs to the inner function, and also wrap the outputs.

> §30 Liquor taxes. Subject to §31 and §32, the retail sale of liquor shall be taxed at 18%.

> §31 Sunday wine. Subject to §32, on Sundays, the retail tax rate of liquor shall be increased by one-third of the existing rate. For example, a 12% rate shall increase by 4% to 16%.

> §32 Public Holidays. On public holidays, the retail sale of liquor shall be subject to a cap of $36. For example, if a purchase of liquor were priced at $300, and taxed at 18%, the cap would reduce the tax payable from $54 to $36.

This is a common function-wrapper pattern which a Haskell programmer will recognize as essentially monadic.

See this conversation for details: <https://claude.ai/share/703d98b0-e339-47ee-9eb5-416c74d272de>
