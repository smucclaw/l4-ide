# L4 Design Principles

L4 is a domain-specific language for law, intended to support
computer-readable formalizations of contracts, legislation, and
regulations.

L4 is intended to be largely written by machine, but reviewed and revised by humans.

LLM AIs are used to *ingest* existing legal documents into L4. The
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

