# L4 Design Principles

L4 is a domain-specific language for law, intended to support
computer-readable formalizations of contracts, legislation, and
regulations.

L4 is intended to be largely written by machine, but reviewed and revised by humans.

LLM AIs are used to _ingest_ existing legal documents into L4. The
logical constructs used in L4 are familiar from the world of
programming, and experiments have shown that LLM AIs (trained for
"instruct" mode operation) are capable of writing L4 code.

L4's language design therefore optimizes for editability and
learnability. Most human uses of L4 involve _tweaking_ existing text
rather than drafting from scratch. Accordingly, L4 can be learned by
looking at existing code. You don't need to read this entire guide to
be productive as a legal engineer, but it will help you understand the
language's syntax and semantics.

Once reviewed and approved by a human domain expert, the L4 version of
a legal text can be translated to serve multiple applications:

- IDE support for drafters
- code generation of end-user-facing web and mobile apps
- visualizations to support comprehension
- chatbot interfaces supporting a conversational mode of engagement

L4 encodings of existing legal agreements are intended to support _isomorphism_: the L4 encoding closely follows the structure and the verbiage of the original text. At the same time, L4 is a _formal_ language, intended to support computer reasoning.

People with a background in programming should find many ideas familiar. Interestingly, people with a background in law should also find many ideas familiar. This is because legal drafters and programmers do similar work: they try to write down a comprehensive specification for how a system with multiple moving parts should behave, and what each of those moving parts ought to do, in a timely fashion. Decisions and actions frequently depend on complicated sets of conditions involving multiple input variables. Good drafters anticipate common problems and talk about how to get things back on track. They wonder what bugs and loopholes might lie latent in the code, and try to imagine scenarios that might put their drafting to the test.

Key differences from mainstream languages such as Python and Javascript:

- L4 is layout-sensitive. Instead of parenthesis, indentation is used for grouping. Instead of commas, list elements are delimited by new lines. This is intended to make L4 more approachable for non-programmers.
- L4 is strongly typed. Algebraic data types are used to improve data modelling and reduce unintentional ambiguity.
- L4 is as much a specification language as it is a programming language. The "letter of the law" can be written down in L4. The "spirit of the law" can also be written in L4, and the "spirit" part can be used to test the "letter".
- Legal idioms are translated, where possible, into corresponding L4 idioms.

L4 is intended to be written not just by humans but by AIs too. L4 ships with extensions for VS Code to support syntax highlighting, type checking, and logic visualization. Copilot is capable of automatically translating existing legal text into L4, reducing the burden of authoring to one of review and correction. Just press TAB.

After a legal agreement or legislation/regulation has been encoded in L4, the L4 suite of tools helps to answer common questions like:

- given certain events that have occurred so far, what state am I in?
- what are my immediate obligations?
- if I want to achieve a certain outcome, what must I do? By when?
- if I want to avoid a certain outcome, what can I do? How soon should I do it?
- if a certain amount of money has been calculated, what was the basis for that calculation?
- if a malicious counterparty wanted to violate the spirit but obey the letter, what might they try to do?

These questions can be answered visually or interactively with a chatbot.
