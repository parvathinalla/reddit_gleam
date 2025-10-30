import gleam/io

pub fn log_event(event: String) {
  let _ = io.println("[EVENT]: " <> event)
}