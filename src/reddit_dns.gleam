import gleam/io

pub fn resolve_node(name: String) {
  let _ = io.println("Resolving node: " <> name)
  // In a real scenario, integrate with Erlang DNS discovery
  name
}