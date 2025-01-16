# mattwaddington

Private repository for a mattwadd-specific dialect of L\*4.

## How To Build

Minimal Requirements:

- Haskell
  - [GHCup](https://www.haskell.org/ghcup/)
  - ghc 9.6.6
  - cabal 3.10 (or newer)
- npm >= 10.9.2
  - installed via `corepack` or `nvm` or your package manager
  - See the README in `frontend/vscode` for more details on working with the VSCode extension.

To build the `mattwaddington` project, all you have to do is this:

```sh
git submodule update --init
cabal update
cabal build all
npm install && npm run build
```

Setting up a development run for the Language Server:

```sh
cabal install exe:jl4-lsp --overwrite-policy=always
# Make sure the installation directory is on the `$PATH`
code .
# Press F5 from within VSCode to launch the JL4 extension
# Open folder ./jl4/examples to see the Language Server in action
```

Running the Haskell tests:

```sh
cabal test test:jl4-test
```

## Quick links

- Drafts of the Dec report for CRLP, started on Tues Dec 17 2024
  - [The google doc draft](https://docs.google.com/drawings/d/1CaJtpHDnRPQAfC2q3jdXjuXObJu9fN2WrJNfuqawHOk/edit)
  - [The Github repo draft](./REPORT.md)
  - [Slack thread](https://smucclaw.slack.com/archives/C080UD79NQH/p1734426129494699)
- [Proposal for CCLaw work for CRLP, as of 4-11-24](https://drive.google.com/file/d/1KdI_usifYqUDmUpRvCxJfj8utIU8hk8J/view?usp=drive_link)
- Google drive "CRLP with Matthew Waddington": <https://drive.google.com/drive/folders/1qO7BAUqlI9aawPXsrVeDBhD-hwdo9GLI>
  - General tip: sort by latest modified
  - "Various emails from mattwadd" megathread (latest 7 Nov): <https://docs.google.com/document/d/1bdRI-qm9HefUD5Ss98qIQE-3TwqE_Q-OmYCb-fLeo6c/edit?pli=1&tab=t.0>
  - Our response thread is called "October 2024 Email Response", but latest update is 1 Nov. Content of that document has been repurposed into "[Proposal for CCLaw work for CRLP, as of 4-11-24, linked above](https://drive.google.com/file/d/1KdI_usifYqUDmUpRvCxJfj8utIU8hk8J/view?usp=drive_link)"

## Other reading materials

- Computer-readable legislation project, main page: <https://osf.io/yzf6x/>
- Meng's thoughts on L4 syntax: <https://docs.google.com/spreadsheets/d/1RW5wLksZY7BBpjkbYzHDAjAgan994nnTdZiqhTFxoJw/edit?gid=1732775477#gid=1732775477>
- CSL
  - Static Analyses of CSL contracts
    - [A Formally Verified Static Analysis Framework for Compositional Contracts](https://core.ac.uk/download/pdf/322819293.pdf)

## Links to specific examples

- Short parsing exercises: <https://osf.io/rjduz>
  - traffic, farming, alcohol, dogs (imaginary); charities, advertising in event (real)
- British Nationality Act: <https://osf.io/mt78r>. Note that they have a redraft of s1 at the end in a more modern style.
- (Fictional) alcohol sales example in the IDE mockup: <https://crlp-jerseyldo.github.io/ilde-mockup/>
- (Fictional) farming act: <https://osf.io/bcnje>
- [Charities (Jersey) Law 2014](https://www.jerseylaw.je/laws/current/l_41_2014) and its [Reg & Orders](https://www.jerseylaw.je/laws/current/search?q=charities&size=n_50_n)
- [Financial Services Ombudsman (Jersey) Law 2014](https://www.jerseylaw.je/laws/current/l_14_2014)
- [Domestic Abuse (Jersey) Law 2022](https://www.jerseylaw.je/laws/current/l_27_2022)

### Questionnaire

- [Sharpening Up your Summing Up](https://supremecourt.nsw.gov.au/documents/Publications/Speeches/2016-Speeches/RAHulme_20160329_QTrails.pdf)
  - Describes the New Zealand / 'Question Trail' method (which is basically what CRLP seems to favor), with examples, and explains why it's helpful
  - Ian Chai, from Dec 12: See especially pages 21 and 16. Not only do they helpfully instantiate the questions with the specific details / facts (which makes it more comprehensible than, e.g., talking about "the accused person" in the abstract), they also provide all the relevant definitions that are needed to answer the questions.
- [A short example of a questionnaire for jurors on "Assault Occasioning Actual Bodily Harm," sent along from Leon Qiu](https://slack-files.com/T012Q6P08BY-F0858EU6J8Y-9fb05d409e)

Related notes from meetings

- Dec 12 meeting: Can think of the questions as tree where the nodes are in effect sets, so that end users can answer questions independently of each other, but some questions depend on other questions. And re providing the relevant definitions, can imagine providing a transitive closure of the definitions.

## Slack thread

Probably contains still some relevant links that aren't here yet. <https://smucclaw.slack.com/archives/C029JEMBKJB/p1726120969098429>

## Relevant examples from other legal DSLs

[The NSW Community Gaming legislation](https://legislation.nsw.gov.au/view/whole/html/inforce/current/sl-2020-0304), formalized in Catala vs. YScript:

- Catala: <https://github.com/CatalaLang/catala-examples/tree/master/NSW_community_gaming>
- YScript: See page 187 of <https://austlii.community/foswiki/pub/DataLex/WebHome/ys-manual.pdf>
