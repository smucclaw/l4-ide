# Core layout-ir library

This is the core IR and state synchronization library for our frontend apps / libraries.

That is, you can think of LirNodes as being an intermediate representation that's neither the underlying data nor the concrete UI 'displayers'.

- It's an IR that's focused on content as opposed to presentation --- it can be rendered in different ways, via different concrete GUIs.
- It makes certain sorts of data synchronization a lot easier.
- And it's intended to be extensible in the set of LirNode types/variants

## License

Apache License 2.0
