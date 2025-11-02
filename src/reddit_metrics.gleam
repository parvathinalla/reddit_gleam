import gleam/io
import gleam/int
import gleam/list
import gleam/float

pub type Metric {
  Metric(event: String, timestamp: Int)
}

pub type DetailedMetrics {
  DetailedMetrics(
  total_operations: Int,
  registers: Int,
  logins: Int,
  joins: Int,
  posts: Int,
  comments: Int,
  votes: Int,
  dms: Int,
  total_time_ms: Int,
  ops_per_second: Float
  )
}

// This function is now silent. It can be used for metrics without cluttering the output.
pub fn log_event(_event: String) {
  Nil
}

pub fn analyze_metrics(metrics: List(Metric)) -> DetailedMetrics {
  let start_time = case list.first(metrics) {
    Ok(m) -> m.timestamp
    Error(_) -> 0
  }
  let end_time = case list.last(metrics) {
    Ok(m) -> m.timestamp
    Error(_) -> 0
  }
  let total_time = end_time - start_time
  let total_ops = list.length(metrics)
  let ops_per_second = case total_time {
    0 -> 0.0
    _ -> int.to_float(total_ops) /. int.to_float(total_time) *. 1000.0
  }

  // Count operation types
  let registers = count_events(metrics, "Register")
  let logins = count_events(metrics, "Login")
  let joins = count_events(metrics, "JoinSub")
  let posts = count_events(metrics, "CreatePost")
  let comments = count_events(metrics, "CreateComment")
  let votes = count_events(metrics, "Vote")
  let dms = count_events(metrics, "SendDirectMessage")

  DetailedMetrics(
  total_ops,
  registers,
  logins,
  joins,
  posts,
  comments,
  votes,
  dms,
  total_time,
  ops_per_second
  )
}

fn count_events(metrics: List(Metric), event_prefix: String) -> Int {
  list.fold(metrics, 0, fn(acc, m) {
    case m {
      Metric(event, _) -> {
        let is_match = case event {
          _ if event == event_prefix -> True
          _ -> {
            // Check if event starts with prefix
            let len = string_length(event_prefix)
            case string_length(event) >= len {
              True -> string_slice(event, 0, len) == event_prefix
              False -> False
            }
          }
        }
        case is_match {
          True -> acc + 1
          False -> acc
        }
      }
    }
  })
}

// Helper for string operations
@external(erlang, "string", "length")
fn string_length(s: String) -> Int

@external(erlang, "string", "slice")
fn string_slice(s: String, start: Int, len: Int) -> String

pub fn report_metrics(metrics: List(Metric)) {
  let detailed = analyze_metrics(metrics)

  io.println("\n╔════════════════════════════════════════════════════════════╗")
  io.println("║           REDDIT SIMULATOR PERFORMANCE REPORT              ║")
  io.println("╚════════════════════════════════════════════════════════════╝")
  io.println("")
  io.println("┌─ Timing Statistics ─────────────────────────────────────┐")
  io.println("│ Total simulation time: " <> pad_right(int.to_string(detailed.total_time_ms) <> " ms", 31) <> "│")
  io.println("│ Operations per second: " <> pad_right(float.to_string(detailed.ops_per_second), 31) <> "│")
  io.println("└──────────────────────────────────────────────────────────┘")
  io.println("")
  io.println("┌─ Operation Breakdown ───────────────────────────────────┐")
  io.println("│ Total operations:      " <> pad_right(int.to_string(detailed.total_operations), 31) <> "│")
  io.println("│ • User registrations:  " <> pad_right(int.to_string(detailed.registers), 31) <> "│")
  io.println("│ • User logins:         " <> pad_right(int.to_string(detailed.logins), 31) <> "│")
  io.println("│ • Subreddit joins:     " <> pad_right(int.to_string(detailed.joins), 31) <> "│")
  io.println("│ • Posts created:       " <> pad_right(int.to_string(detailed.posts), 31) <> "│")
  io.println("│ • Comments created:    " <> pad_right(int.to_string(detailed.comments), 31) <> "│")
  io.println("│ • Votes cast:          " <> pad_right(int.to_string(detailed.votes), 31) <> "│")
  io.println("│ • Direct messages:     " <> pad_right(int.to_string(detailed.dms), 31) <> "│")
  io.println("└──────────────────────────────────────────────────────────┘")
  io.println("")
}

fn pad_right(s: String, width: Int) -> String {
  let len = string_length(s)
  case len >= width {
    True -> s
    False -> {
      let spaces = width - len
      s <> repeat_string(" ", spaces)
    }
  }
}

fn repeat_string(s: String, n: Int) -> String {
  case n <= 0 {
    True -> ""
    False -> s <> repeat_string(s, n - 1)
  }
}