import gleam/io
import reddit_simulator


pub fn main() {
let _ = io.println("Starting Reddit simulator with 100 users...")
reddit_simulator.run_simulator(100)
}