# JL4 Web Sessions

`jl4-websessions` is a SQLite-backed persistence service for the browser-based L4 playground. Users can save their L4 programs and retrieve them later by UUID.

## Usage

```bash
cabal run jl4-websessions -- PORT DB_PATH
# e.g.
cabal run jl4-websessions -- 8002 /tmp/sessions.db
```

## API

- `POST /` — save an L4 program (body: JSON string with L4 source), returns a UUID
- `GET /?id={uuid}` — retrieve a saved program
- `GET /session` — list session UUIDs (returns JSON array)

## Session permalinks

Every saved program gets a randomly generated UUID which becomes a shareable link. Bookmark or send the URL to collaborators and they can load the session by UUID.
