# reddit_gleam

[![Package Version](https://img.shields.io/hexpm/v/reddit_gleam)](https://hex.pm/packages/reddit_gleam)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/reddit_gleam/)

```sh
gleam add reddit_gleam@1
```
```gleam
import reddit_gleam

pub fn main() -> Nil {
  // TODO: An example of the project in use
}
```

Further documentation can be found at <https://hexdocs.pm/reddit_gleam>.

## Development

```powershell
# Run unit tests
gleam test

# Build
gleam build

# Run the Erlang wrapper to spawn N clients (example: 5)
# Make sure _build/default/lib/*/ebin is on the Erlang code path
erl -pa _build/default/lib/*/ebin -noshell -s reddit_server start_and_spawn 5 -s init stop
```

In addition to the existing README content, this project includes:

- `src/reddit_simulator.gleam` — an in-process simulator (`run_simulator`) that registers users, joins a subreddit, creates posts, comments, and votes.
- `src/reddit_server.erl` — an Erlang wrapper that runs the engine as a BEAM process and exposes `start_and_spawn/1` to spawn simple Erlang clients.

If you want a tiny runner (a `main` that calls `reddit_simulator.run_simulator(10)`), I can add it.
