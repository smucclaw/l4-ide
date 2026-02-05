# Proposal

## The main suggestion

I don't think we should build a 'typical' chatbot (like Thomas' version), where there's a lot of latitude over
how the user can interact with the LLM.

What I'd like to propose instead is moving to a mode of interaction that is

- more constrained, more like what Anthropic calls a 'workflow',
- and more focused on JL4 and its capabilities

### The simplest version of this

The absolute simplest version of this would be:

- LLM(s) are used only as _semantic parsers_ that convert unstructured text to structured calls of pre-defined functions in our API (i.e., no free-form conversation)

#### I.e., the interaction pattern would look like this:

The part that is similar to the usual chatbot:

- [ Landing page with the input box]
- User can ask a question --- 'submit a natural language query' (in addition to, e.g., clicking on pre-defined options / 'tools').
- There's flexibility in exactly what should happen next, but the broad idea is this. The AI/machine-learning model will try to figure out if there's a jl4 function definition from the API that's relevant and 'parse' this natural language query to a function call if so (Thomas' chatbot has a version of this too)
  - If the model is not confident enough about finding a relevant function or parsing to a function call, it could ask the user for more info
  - And if we end up developing the structured questionnaire capabilities we had talked about, that could be yet another option/'tool' available to the model.

Where this approach differs from the chatbot:

- **At any point in time, the llm will only be able to choose from a constrained set of options / templates** (and similarly with the user), e.g.
  - 'Can't figure out what tool to call'
  - 'I think I know what function is relevant, but I'm not sure how to translate the arguments'
  - 'Here's the function call and the result'
  - 'Here's a (constrained) menu of follow-up options you, the user, can take now that we have a result'
- For example, on what I have in mind, what happens after we get the response is a lot more constrained than with a chatbot: see https://www.tldraw.com/f/D9dnHzRS_NYRelsbOzmqm?d=v-1029.-631.2681.2408.D62awbu4D0_af-xZx8qEo for a quick wireframe mockup
- There will still be interesting, interactive things the user can do: e.g. LSP-y things that might dovetail nicely with our NLG efforts, or visualizing things and seeing how the results change as the inputs change.
- It's just that the user now cannot go off on any tangent with the chatbot, including having discussions completely unrelated to JL4 or law

### The motivation behind this proposal

#### 1. A semantic parser seems like a more natural fit for an API that evaluates pre-defined functions than a chat UX

What would a chat UX be able to buy you, when it comes to using the current eval-predefined-functions API, that a more constrained setup cannot?

And in particular, _for this usecase_, with these usecase partners?

#### 2. Synergy with our other efforts

The hope is that this would align with our work not just on the decision service, but also on, e.g., the NLG annotations, references/citations (which could be displayed, e.g., in a nice hover in the Results view and be further interacted with by the user), the structured Q&A we had been talking about doing, and the communication between the visualizer and the evaluator.

#### 3. It's better to start with a simpler, more constrained, and more reliable, use of machine learning / AI, and then iteratively improve

There are other relatively simple things we could add on the AI/ML side in the future, if this doesn't feel like it has enough AI.
For instance, we could also allow users to ask questions about images/photos (e.g. a photo of a speeding ticket), using multimodal models.

### Other examples in this genre

- [Honeycomb Query Builder](https://docs.honeycomb.io/reference/honeycomb-ui/query/#query-builder)
  - A blog post: https://www.honeycomb.io/blog/hard-stuff-nobody-talks-about-llm
- A toy example of a natural language to SQL query parser: https://natural-language-postgres.vercel.app/

### Questions

- I'm not sure how feasible some of these ideas are -- e.g. having LSP-like functionality in what we show to the user

### Breaking down the further work

Frontend, LLM orchestration

- Building a frontend and llm orchestrator that implements these more constrained interaction pattern(s)

Backend

- Decision service API
  - Improving the traces:
    - Having the traces be structured
    - Making them less verbose (e.g. with heuristics)
  - Improving the schemas / types in the decision service API (e.g. for the function parameters, and deciding how we want to expose the return type)
- Communication between the visualizer and the evaluator

Writing JL4

- Writing more relevant L4 example programs, and across a range of legislation

LLM orchestration, data science

- Potential data science tasks include:
  - systematically improving the retrieval of the function declaration
  - improving when we should ask the user to tell us which function or to supply the args instead of trying to guess, or when to do a 'guided/structured questionnaire'
  - relatedly, finetuning the LLM(s) to construct these function calls (and coming up with a recipe to let other people easily do so too).
- We could prioritize other work. But if we do want to work on this, this is something Chong Cher and I could work on.
