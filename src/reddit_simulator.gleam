import reddit_engine
import reddit_types
import gleam/list
import gleam/int
import gleam/io
import zipf
import reddit_metrics

@external(erlang, "os", "timestamp")
fn os_timestamp() -> #(Int, Int, Int)

fn now_ms() -> Int {
  let #(mega, sec, micro) = os_timestamp()
  mega * 1_000_000 * 1_000   // mega seconds -> ms
  + sec * 1_000              // seconds -> ms
  + micro / 1_000            // microseconds -> ms
}


// Small accumulator type for the simulator
pub type SimAcc {
  SimAcc(state: reddit_engine.EngineState, last_post: Int, metrics: List(reddit_metrics.Metric))
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
  let subreddits = ["r/gleam", "r/programming", "r/gaming", "r/news", "r/memes"]

  // Helper to unwrap the returned EngineResult state (always returns EngineResult)
  let unwrap = fn(res, metrics, event) {
    let timestamp = now_ms()
    let new_metrics = list.append(metrics, [reddit_metrics.Metric(event, timestamp)])
    case res { reddit_engine.EngineResult(s, _) -> #(s, new_metrics) }
  }

  let final = list.fold(list.range(1, user_count), SimAcc(initial_state, 0, list.new()), fn(acc, i) {
    case acc {
      SimAcc(state, last_post_id, metrics) -> {
        let name = "user_" <> int.to_string(i)

        // Register, login, join, create post sequentially; fall back to current state on unexpected replies
        let #(s1, m1) = unwrap(reddit_engine.handle_message(state, reddit_types.Register(name, "pw")), metrics, "Register")
        let #(s2, m2) = unwrap(reddit_engine.handle_message(s1, reddit_types.Login(name, "pw")), m1, "Login")
        let subreddit_name = get_at(subreddits, i % list.length(subreddits) + 1)
        let #(s3, m3) = unwrap(reddit_engine.handle_message(s2, reddit_types.JoinSub(name, subreddit_name)), m2, "JoinSub")
        let #(s4, m4) = unwrap(reddit_engine.handle_message(s3, reddit_types.CreatePost(name, subreddit_name, "Hello from " <> name, "simulated post")), m3, "CreatePost")

        // new post id is s4.post_id_counter
  case s4 { reddit_engine.EngineState(_users, _subs, _votes, _global_posts, post_ctr, _cctr, _ops) ->
          case last_post_id == 0 {
            True -> SimAcc(s4, post_ctr, m4)
            False -> {
              // comment and upvote previous post; if any step fails, keep going with most recent state
              let #(s5, m5) = unwrap(reddit_engine.handle_message(s4, reddit_types.CreateComment(name, last_post_id, 0, "Nice!")), m4, "CreateComment")
              let #(s6, m6) = unwrap(reddit_engine.handle_message(s5, reddit_types.Vote(name, last_post_id, 1)), m5, "Vote")
              SimAcc(s6, post_ctr, m6)
            }
          }
        }
      }
    }
  })

  case final { SimAcc(state_final, last, metrics) -> {
    let _ = io.println("Simulation complete. Operations=" <> int.to_string(state_final.operations) <> " last_post=" <> int.to_string(last))
    // Test DM functionality
    let #(s_dm1, m_dm1) = unwrap(reddit_engine.handle_message(state_final, reddit_types.SendDirectMessage("user_1", "user_2", "Hello user_2!")), metrics, "SendDirectMessage")
    let #(s_dm2, m_dm2) = unwrap(reddit_engine.handle_message(s_dm1, reddit_types.GetDirectMessages("user_2")), m_dm1, "GetDirectMessages")
    let res_dm2 = reddit_engine.handle_message(s_dm2, reddit_types.GetDirectMessages("user_2"))

    case res_dm2 {
      reddit_engine.EngineResult(_, reply) -> case reply {
        reddit_types.DirectMessages(msgs) -> {
          io.println("User_2 inbox:")
          list.each(msgs, fn(dm) { io.println("  From: " <> dm.from <> ", Body: " <> dm.body) })
        }
        _ -> io.println("Error getting user_2 messages")
      }
    }
    reddit_metrics.report_metrics(m_dm2)
  } }
}

// Run a simulator that creates `sub_count` subreddits and assigns users to
// them according to a Zipf(s) distribution. Uses a simple seeded Zipf sampler
// so behavior is deterministic when desired.
pub fn run_simulator_with_distribution(user_count: Int, sub_count: Int, s: Int) {

  let initial_state = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)

  // create a list of subreddit names r/1 .. r/sub_count
  let subs = list.map(list.range(1, sub_count), fn(i) { "r/" <> int.to_string(i) })

  let final = list.fold(list.range(1, user_count), SimAcc(initial_state, 0, list.new()), fn(acc, i) {
    case acc {
      SimAcc(state, last_post_id, metrics) -> {
        let name = "user_" <> int.to_string(i)

        let unwrap = fn(res, metrics, event) {
          let timestamp = now_ms()
          let new_metrics = list.append(metrics, [reddit_metrics.Metric(event, timestamp)])
          case res { reddit_engine.EngineResult(s, _) -> #(s, new_metrics) }
        }

        let #(s1, m1) = unwrap(reddit_engine.handle_message(state, reddit_types.Register(name, "pw")), metrics, "Register")
        let #(s2, m2) = unwrap(reddit_engine.handle_message(s1, reddit_types.Login(name, "pw")), m1, "Login")

        // pick a subreddit index via Zipf seeded by user index
  let idx = zipf.sample_zipf_seed(s, sub_count, i)
    // sample_zipf_seed returns a 1-based index; use get_at helper above
    let subname = get_at(subs, idx)

        let #(s3, m3) = unwrap(reddit_engine.handle_message(s2, reddit_types.JoinSub(name, subname)), m2, "JoinSub")
        let #(s4, m4) = unwrap(reddit_engine.handle_message(s3, reddit_types.CreatePost(name, subname, "Hello from " <> name, "simulated post")), m3, "CreatePost")

  case s4 { reddit_engine.EngineState(_users, _subs, _votes, _global_posts, post_ctr, _cctr, _ops) ->
          case last_post_id == 0 {
            True -> SimAcc(s4, post_ctr, m4)
            False -> {
              let #(s5, m5) = unwrap(reddit_engine.handle_message(s4, reddit_types.CreateComment(name, last_post_id, 0, "Nice!")), m4, "CreateComment")
              let #(s6, m6) = unwrap(reddit_engine.handle_message(s5, reddit_types.Vote(name, last_post_id, 1)), m5, "Vote")
              SimAcc(s6, post_ctr, m6)
            }
          }
        }
      }
    }
  })

  case final { SimAcc(state_final, last, metrics) -> {
    let _ = io.println("Simulation complete. Operations=" <> int.to_string(state_final.operations) <> " last_post=" <> int.to_string(last))
    reddit_metrics.report_metrics(metrics)
  } }
}
