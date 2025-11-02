import gleam/io
import gleam/int
import gleam/list
import gleam/float

pub type Metric {
  Metric(event: String, timestamp: Int)
}
pub fn log_event(event: String) {
  let _ = io.println("[EVENT]: " <> event)
}

pub fn report_metrics(metrics: List(Metric)) {
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

  io.println("\n--- Performance Report ---")
  io.println("Total simulation time: " <> int.to_string(total_time) <> "ms")
  io.println("Total operations recorded: " <> int.to_string(total_ops))
  io.println("Operations per second: " <> float.to_string(ops_per_second))
}
