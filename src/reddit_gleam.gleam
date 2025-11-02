import gleam/io
import reddit_simulator

pub fn main() {
  io.println("╔════════════════════════════════════════════════════════════╗")
  io.println("║         REDDIT CLONE SIMULATOR - GLEAM IMPLEMENTATION     ║")
  io.println("╚════════════════════════════════════════════════════════════╝")
  io.println("")

  // Run basic simulator with 100 users
  io.println("═══ Running Basic Simulator (100 users) ═══")
  reddit_simulator.run_simulator(100)

  io.println("\n\n")

  // Run advanced simulator with Zipf distribution, disconnections, and re-posts
  io.println("═══ Running Advanced Simulator with Zipf Distribution ═══")
  reddit_simulator.run_simulator_with_distribution(200, 20, 2)

  io.println("\n╔════════════════════════════════════════════════════════════╗")
  io.println("║                 ALL SIMULATIONS COMPLETE                   ║")
  io.println("╚════════════════════════════════════════════════════════════╝")
}