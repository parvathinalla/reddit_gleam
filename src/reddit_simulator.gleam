import reddit_engine
import reddit_types
import gleam/list
import gleam/int
import gleam/io
import zipf

// Small accumulator type for the simulator
pub type SimAcc {
  SimAcc(state: reddit_engine.EngineState, last_post: Int)
}

// Helper: get element at 1-based index from a list (returns empty string if out of range)
fn get_at(xs, idx) {
  case xs {
    [] -> ""
  [h, ..t] -> case idx <= 1 {
      True -> h
      False -> get_at(t, idx - 1)
    }
  }
}

// Simple simulator that registers and logs in users, has them join a subreddit and create a post.
pub fn run_simulator(user_count: Int) {

  // start with an empty engine state and no last-post tracked
  let initial_state = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)

  let final = list.fold(list.range(1, user_count), SimAcc(initial_state, 0), fn(acc, i) {
    case acc {
      SimAcc(state, last_post_id) -> {
        let name = "user_" <> int.to_string(i)

        // Helper to unwrap the returned EngineResult state (always returns EngineResult)
        let unwrap = fn(res) {
          case res { reddit_engine.EngineResult(s, _) -> s }
        }

        // Register, login, join, create post sequentially; fall back to current state on unexpected replies
        let s1 = unwrap(reddit_engine.handle_message(state, reddit_types.Register(name, "pw")))
        let s2 = unwrap(reddit_engine.handle_message(s1, reddit_types.Login(name, "pw")))
        let s3 = unwrap(reddit_engine.handle_message(s2, reddit_types.JoinSub(name, "r/sim")))
        let s4 = unwrap(reddit_engine.handle_message(s3, reddit_types.CreatePost(name, "r/sim", "Hello from " <> name, "simulated post")))

        // new post id is s4.post_id_counter
  case s4 { reddit_engine.EngineState(_users, _subs, _votes, _global_posts, post_ctr, _cctr, _ops) ->
          case last_post_id == 0 {
            True -> SimAcc(s4, post_ctr)
            False -> {
              // comment and upvote previous post; if any step fails, keep going with most recent state
              let s5 = unwrap(reddit_engine.handle_message(s4, reddit_types.CreateComment(name, last_post_id, 0, "Nice!")))
              let s6 = unwrap(reddit_engine.handle_message(s5, reddit_types.Vote(name, last_post_id, 1)))
              SimAcc(s6, post_ctr)
            }
          }
        }
      }
    }
  })

  case final { SimAcc(state_final, last) -> {
    let _ = io.println("Simulation complete. Operations=" <> int.to_string(state_final.operations) <> " last_post=" <> int.to_string(last))
  } }
}

// Run a simulator that creates `sub_count` subreddits and assigns users to
// them according to a Zipf(s) distribution. Uses a simple seeded Zipf sampler
// so behavior is deterministic when desired.
pub fn run_simulator_with_distribution(user_count: Int, sub_count: Int, s: Int) {

  let initial_state = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)

  // create a list of subreddit names r/1 .. r/sub_count
  let subs = list.map(list.range(1, sub_count), fn(i) { "r/" <> int.to_string(i) })

  let final = list.fold(list.range(1, user_count), SimAcc(initial_state, 0), fn(acc, i) {
    case acc {
      SimAcc(state, last_post_id) -> {
        let name = "user_" <> int.to_string(i)

        let unwrap = fn(res) {
          case res { reddit_engine.EngineResult(s, _) -> s }
        }

        let s1 = unwrap(reddit_engine.handle_message(state, reddit_types.Register(name, "pw")))
        let s2 = unwrap(reddit_engine.handle_message(s1, reddit_types.Login(name, "pw")))

        // pick a subreddit index via Zipf seeded by user index
  let idx = zipf.sample_zipf_seed(s, sub_count, i)
    // sample_zipf_seed returns a 1-based index; use get_at helper above
    let subname = get_at(subs, idx)

        let s3 = unwrap(reddit_engine.handle_message(s2, reddit_types.JoinSub(name, subname)))
        let s4 = unwrap(reddit_engine.handle_message(s3, reddit_types.CreatePost(name, subname, "Hello from " <> name, "simulated post")))

  case s4 { reddit_engine.EngineState(_users, _subs, _votes, _global_posts, post_ctr, _cctr, _ops) ->
          case last_post_id == 0 {
            True -> SimAcc(s4, post_ctr)
            False -> {
              let s5 = unwrap(reddit_engine.handle_message(s4, reddit_types.CreateComment(name, last_post_id, 0, "Nice!")))
              let s6 = unwrap(reddit_engine.handle_message(s5, reddit_types.Vote(name, last_post_id, 1)))
              SimAcc(s6, post_ctr)
            }
          }
        }
      }
    }
  })

  case final { SimAcc(state_final, last) -> {
    let _ = io.println("Simulation complete. Operations=" <> int.to_string(state_final.operations) <> " last_post=" <> int.to_string(last))
  } }
}