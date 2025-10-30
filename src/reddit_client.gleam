import gleam/io
import reddit_types

pub type Client {
  Client(
    name: String,
    config: reddit_types.ClientConfig
  )
}

pub fn simulate_user(name: String) {
  let _client = create_client(name)
  let _ = io.println("Simulating user: " <> name)
  // Simulate behavior like joining, posting, etc.
}

pub fn create_client(name: String) -> Client {
  Client(
    name: name,
    config: reddit_types.ClientConfig(
      ai_model: "claude-sonnet-3.5",
      ai_model_enabled: True
    )
  )
}
