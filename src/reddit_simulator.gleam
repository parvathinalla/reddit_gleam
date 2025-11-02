import gleam/float
import gleam/string
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

pub type UserState {
  Online
  Offline
}

pub type SimulatorUser {
  SimulatorUser(
  name: String,
  state: UserState,
  last_active: Int,
  post_count: Int,
  subreddit: String
  )
}

// Small accumulator type for the simulator
pub type SimAcc {
  SimAcc(
  state: reddit_engine.EngineState,
  last_post: Int,
  metrics: List(reddit_metrics.Metric),
  users: List(SimulatorUser)
  )
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
  io.println("Created subreddits: " <> string.join(subreddits, ", "))

  // Helper to unwrap the returned EngineResult state (always returns EngineResult)
  let unwrap = fn(res, metrics, event) {
    let timestamp = now_ms()
    let new_metrics = list.append(metrics, [reddit_metrics.Metric(event, timestamp)])
    case res { reddit_engine.EngineResult(s, _) -> #(s, new_metrics) }
  }

  let final = list.fold(list.range(1, user_count), SimAcc(initial_state, 0, list.new(), list.new()), fn(acc, i) {
    case acc {
      SimAcc(state, last_post_id, metrics, _users) -> {
        let name = "user_" <> int.to_string(i)

        // Register, login, join, create post sequentially
        let #(s1, m1) = unwrap(reddit_engine.handle_message(state, reddit_types.Register(name, "pw")), metrics, "Register")
        let #(s2, m2) = unwrap(reddit_engine.handle_message(s1, reddit_types.Login(name, "pw")), m1, "Login")
        let subreddit_name = get_at(subreddits, i % list.length(subreddits) + 1)
        let #(s3, m3) = unwrap(reddit_engine.handle_message(s2, reddit_types.JoinSub(name, subreddit_name)), m2, "JoinSub")
        let #(s4, m4) = unwrap(reddit_engine.handle_message(s3, reddit_types.CreatePost(name, subreddit_name, "Hello from " <> name, "simulated post")), m3, "CreatePost")

        // Print a summary for a few users to keep the output clean
        case i <= 5 {
          True -> io.println("  - User " <> name <> " joined " <> subreddit_name <> " and created a post.")
          False -> Nil
        }

        // new post id is s4.post_id_counter
        case s4 { reddit_engine.EngineState(_users, _subs, _votes, _global_posts, post_ctr, _cctr, _ops) ->
        case last_post_id == 0 {
          True -> SimAcc(s4, post_ctr, m4, list.new())
          False -> {
            // comment and upvote previous post
            let #(s5, m5) = unwrap(reddit_engine.handle_message(s4, reddit_types.CreateComment(name, last_post_id, 0, "Nice!")), m4, "CreateComment")
            let #(s6, m6) = unwrap(reddit_engine.handle_message(s5, reddit_types.Vote(name, last_post_id, 1)), m5, "Vote")
            SimAcc(s6, post_ctr, m6, list.new())
          }
        }
        }
      }
    }
  })

  case final { SimAcc(state_final, last, metrics, _) -> {
    io.println("\n...and so on for " <> int.to_string(user_count) <> " users.")
    io.println("\n--- Simulation Summary ---")
    io.println("Total users simulated: " <> int.to_string(user_count))
    io.println("Total posts created: " <> int.to_string(last))

    // Test DM functionality
    io.println("\n--- Direct Message Test ---")
    let #(s_dm1, m_dm1) = unwrap(reddit_engine.handle_message(state_final, reddit_types.SendDirectMessage("user_1", "user_2", "Hello user_2!")), metrics, "SendDirectMessage")
    io.println("  - user_1 sent a direct message to user_2.")
    let #(_s_dm2, m_dm2) = unwrap(reddit_engine.handle_message(s_dm1, reddit_types.GetDirectMessages("user_2")), m_dm1, "GetDirectMessages")
    let res_dm2 = reddit_engine.handle_message(s_dm1, reddit_types.GetDirectMessages("user_2"))

    case res_dm2 {
      reddit_engine.EngineResult(_, reply) -> case reply {
        reddit_types.DirectMessages(msgs) -> {
          io.println("  - user_2's inbox check:")
          list.each(msgs, fn(dm) { io.println("    - From: " <> dm.from <> ", Body: '" <> dm.body <> "'") })
        }
        _ -> io.println("Error getting user_2 messages")
      }
    }
    reddit_metrics.report_metrics(m_dm2)
  } }
}

// Run a simulator with Zipf distribution and disconnection/reconnection cycles
pub fn run_simulator_with_distribution(user_count: Int, sub_count: Int, zipf_s: Int) {
  io.println("\n╔═══════════════════════════════════════════════════════════╗")
  io.println("║    ADVANCED REDDIT SIMULATOR WITH ZIPF DISTRIBUTION       ║")
  io.println("╚═══════════════════════════════════════════════════════════╝")
  io.println("")
  io.println("Configuration:")
  io.println("  • Total users: " <> int.to_string(user_count))
  io.println("  • Total subreddits: " <> int.to_string(sub_count))
  io.println("  • Zipf exponent: " <> int.to_string(zipf_s))
  io.println("")

  let initial_state = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)

  // create a list of subreddit names r/1 .. r/sub_count
  let subs = list.map(list.range(1, sub_count), fn(i) { "r/" <> int.to_string(i) })

  let unwrap = fn(res, metrics, event) {
    let timestamp = now_ms()
    let new_metrics = list.append(metrics, [reddit_metrics.Metric(event, timestamp)])
    case res { reddit_engine.EngineResult(s, _) -> #(s, new_metrics) }
  }

  // Phase 1: User registration and initial join
  io.println("Phase 1: User registration and subreddit joining...")
  let phase1 = list.fold(list.range(1, user_count), SimAcc(initial_state, 0, list.new(), list.new()), fn(acc, i) {
    case acc {
      SimAcc(state, last_post_id, metrics, users) -> {
        let name = "user_" <> int.to_string(i)

        let #(s1, m1) = unwrap(reddit_engine.handle_message(state, reddit_types.Register(name, "pw")), metrics, "Register")
        let #(s2, m2) = unwrap(reddit_engine.handle_message(s1, reddit_types.Login(name, "pw")), m1, "Login")

        // pick a subreddit index via Zipf seeded by user index
        let idx = zipf.sample_zipf_seed(zipf_s, sub_count, i)
        let subname = get_at(subs, idx)

        let #(s3, m3) = unwrap(reddit_engine.handle_message(s2, reddit_types.JoinSub(name, subname)), m2, "JoinSub")

        // Create simulator user record
        let sim_user = SimulatorUser(name, Online, now_ms(), 0, subname)
        let new_users = list.append(users, [sim_user])

        case i % 20 == 0 {
          True -> io.println("  Registered " <> int.to_string(i) <> " users...")
          False -> Nil
        }

        SimAcc(s3, last_post_id, m3, new_users)
      }
    }
  })

  // Phase 2: Simulate posting with Zipf distribution (popular subs get more posts)
  io.println("\nPhase 2: Simulating posts (Zipf-distributed activity)...")
  let phase2 = simulate_zipf_posts(phase1, subs, zipf_s, sub_count)

  // Phase 3: Simulate disconnection/reconnection cycles
  io.println("\nPhase 3: Simulating disconnection/reconnection cycles...")
  let phase3 = simulate_disconnection_cycles(phase2, 3)

  // Phase 4: Simulate re-posts (FULLY IMPLEMENTED)
  io.println("\nPhase 4: Simulating re-posts from popular content...")
  let final = simulate_reposts(phase3)

  // Final statistics
  case final { SimAcc(state_final, _, metrics, users) -> {
    io.println("\n╔═══════════════════════════════════════════════════════════╗")
    io.println("║                  SIMULATION COMPLETE                       ║")
    io.println("╚═══════════════════════════════════════════════════════════╝")

    // Print subreddit distribution
    print_subreddit_distribution(state_final, subs)

    // Print user activity distribution
    print_user_activity(users)

    reddit_metrics.report_metrics(metrics)
  } }
}

// Simulate posts with Zipf distribution: popular subreddits get more posts
fn simulate_zipf_posts(acc: SimAcc, subs: List(String), zipf_s: Int, sub_count: Int) -> SimAcc {
  case acc {
    SimAcc(state, last_post_id, metrics, users) -> {
      list.fold(users, SimAcc(state, last_post_id, metrics, users), fn(inner_acc, user) {
        case inner_acc {
          SimAcc(inner_state, last_id, m, u) -> {
            case user {
              SimulatorUser(name, Online, _, post_count, subreddit) -> {
                // Users in popular subreddits post more (Zipf distribution)
                let sub_idx = find_subreddit_index(subs, subreddit)
                let post_frequency = calculate_post_frequency(sub_idx, zipf_s, sub_count)

                // Create posts based on frequency
                let result = create_multiple_posts(inner_state, name, subreddit, post_frequency, last_id, m)
                case result {
                  #(new_state, new_last_id, new_metrics) -> {
                    let updated_user = SimulatorUser(name, Online, now_ms(), post_count + post_frequency, subreddit)
                    let updated_users = update_user_in_list(u, updated_user)
                    SimAcc(new_state, new_last_id, new_metrics, updated_users)
                  }
                }
              }
              SimulatorUser(_, Offline, _, _, _) -> inner_acc
            }
          }
        }
      })
    }
  }
}

// Simulate disconnection and reconnection cycles
fn simulate_disconnection_cycles(acc: SimAcc, cycles: Int) -> SimAcc {
  case cycles <= 0 {
    True -> acc
    False -> {
      case acc {
        SimAcc(state, last_post_id, metrics, users) -> {
          io.println("  Cycle " <> int.to_string(4 - cycles) <> ": Disconnecting 30% of users...")

          // Disconnect 30% of users
          let disconnected_users = disconnect_users(users, 30)

          // Simulate some activity from online users
          let active_result = simulate_online_activity(SimAcc(state, last_post_id, metrics, disconnected_users))

          case active_result {
            SimAcc(new_state, new_last_id, new_metrics, active_users) -> {
              io.println("  Cycle " <> int.to_string(4 - cycles) <> ": Reconnecting users...")

              // Reconnect all users
              let reconnected_users = reconnect_all_users(active_users)

              // Continue with next cycle
              simulate_disconnection_cycles(
              SimAcc(new_state, new_last_id, new_metrics, reconnected_users),
              cycles - 1
              )
            }
          }
        }
      }
    }
  }
}

// Disconnect a percentage of users
fn disconnect_users(users: List(SimulatorUser), percentage: Int) -> List(SimulatorUser) {
  list.index_map(users, fn(user, idx) {
    case user {
      SimulatorUser(name, _, last_active, post_count, subreddit) -> {
        // Use index to deterministically disconnect percentage of users
        case { idx * 100 / list.length(users) } < percentage {
          True -> SimulatorUser(name, Offline, last_active, post_count, subreddit)
          False -> user
        }
      }
    }
  })
}

// Reconnect all users
fn reconnect_all_users(users: List(SimulatorUser)) -> List(SimulatorUser) {
  list.map(users, fn(user) {
    case user {
      SimulatorUser(name, _, _, post_count, subreddit) ->
      SimulatorUser(name, Online, now_ms(), post_count, subreddit)
    }
  })
}

// Simulate activity from online users only
fn simulate_online_activity(acc: SimAcc) -> SimAcc {
  case acc {
    SimAcc(_state, _last_post_id, _metrics, users) -> {
      let unwrap = fn(res, metrics_arg, event) {
        let timestamp = now_ms()
        let new_metrics = list.append(metrics_arg, [reddit_metrics.Metric(event, timestamp)])
        case res { reddit_engine.EngineResult(s, _) -> #(s, new_metrics) }
      }

      let online_users = list.filter(users, fn(u) {
        case u {
          SimulatorUser(_, Online, _, _, _) -> True
          _ -> False
        }
      })

      io.println("    " <> int.to_string(list.length(online_users)) <> " users online, creating activity...")

      // Each online user creates one post and votes on the last post
      list.fold(online_users, acc, fn(inner_acc, user) {
        case inner_acc {
          SimAcc(inner_state, last_id, m, u) -> {
            case user {
              SimulatorUser(name, Online, _, post_count, subreddit) -> {
                let #(s1, m1) = unwrap(
                reddit_engine.handle_message(inner_state, reddit_types.CreatePost(name, subreddit, "Post from " <> name, "Content")),
                m,
                "CreatePost"
                )

                case s1 {
                  reddit_engine.EngineState(_, _, _, _, new_post_id, _, _) -> {
                    // Vote on previous post if exists
                    let #(s2, m2) = case last_id > 0 {
                      True -> unwrap(
                      reddit_engine.handle_message(s1, reddit_types.Vote(name, last_id, 1)),
                      m1,
                      "Vote"
                      )
                      False -> #(s1, m1)
                    }

                    let updated_user = SimulatorUser(name, Online, now_ms(), post_count + 1, subreddit)
                    let updated_users = update_user_in_list(u, updated_user)
                    SimAcc(s2, new_post_id, m2, updated_users)
                  }
                }
              }
              _ -> inner_acc
            }
          }
        }
      })
    }
  }
}

// FULLY IMPLEMENTED: Simulate re-posts from popular content
fn simulate_reposts(acc: SimAcc) -> SimAcc {
  case acc {
    SimAcc(state, last_post_id, metrics, users) -> {
      io.println("  Finding top posts to re-post...")

      // Find top 10 posts by score from global_posts
      let top_posts = get_top_posts(state.global_posts, 10)

      io.println("  Found " <> int.to_string(list.length(top_posts)) <> " popular posts")

      // Have 20% of users re-post popular content
      let reposters = list.take(users, list.length(users) / 5)

      io.println("  " <> int.to_string(list.length(reposters)) <> " users will create re-posts...")

      let unwrap = fn(res, metrics_arg, event) {
        let timestamp = now_ms()
        let new_metrics = list.append(metrics_arg, [reddit_metrics.Metric(event, timestamp)])
        case res { reddit_engine.EngineResult(s, _) -> #(s, new_metrics) }
      }

      // Each reposter picks a random top post and re-posts it to their subreddit
      let result = list.fold(reposters, #(state, last_post_id, metrics), fn(acc_inner, user) {
        case acc_inner {
          #(inner_state, _last_id, m) -> {
            case user {
              SimulatorUser(name, _, _, _, subreddit) -> {
                // Pick a post to repost (use deterministic selection)
                let post_to_repost = pick_post_for_repost(top_posts, subreddit)

                case post_to_repost {
                  reddit_types.Post(_, orig_author, orig_sub, orig_title, orig_body, _, _, _) -> {
                    // Create repost with "[REPOST]" prefix
                    let repost_title = "[REPOST from " <> orig_sub <> "] " <> orig_title
                    let repost_body = "Originally by u/" <> orig_author <> ": " <> orig_body

                    let #(s1, m1) = unwrap(
                    reddit_engine.handle_message(
                    inner_state,
                    reddit_types.CreatePost(name, subreddit, repost_title, repost_body)
                    ),
                    m,
                    "CreatePost[REPOST]"
                    )

                    case s1 {
                      reddit_engine.EngineState(_, _, _, _, new_post_id, _, _) -> {
                        #(s1, new_post_id, m1)
                      }
                    }
                  }
                }
              }
            }
          }
        }
      })

      case result {
        #(final_state, final_last_id, final_metrics) -> {
          io.println("  Re-post simulation complete: " <> int.to_string(list.length(reposters)) <> " re-posts created")

          // Update users with repost activity
          let updated_users = list.map(users, fn(u) {
            case list.any(reposters, fn(r) {
              case r {
                SimulatorUser(rname, _, _, _, _) -> case u {
                  SimulatorUser(uname, _, _, _, _) -> rname == uname
                }
              }
            }) {
              True -> case u {
                SimulatorUser(name, state_u, last_active, post_count, sub) ->
                SimulatorUser(name, state_u, last_active, post_count + 1, sub)
              }
              False -> u
            }
          })

          SimAcc(final_state, final_last_id, final_metrics, updated_users)
        }
      }
    }
  }
}

// Get top N posts sorted by score
fn get_top_posts(posts: List(reddit_types.Post), n: Int) -> List(reddit_types.Post) {
  let sorted = sort_posts_by_score_desc(posts)
  list.take(sorted, n)
}

// Sort posts by score descending
fn sort_posts_by_score_desc(posts: List(reddit_types.Post)) -> List(reddit_types.Post) {
  list.fold(posts, list.new(), fn(acc, p) { insert_sorted_by_score(acc, p) })
}

fn insert_sorted_by_score(sorted_acc: List(reddit_types.Post), p: reddit_types.Post) -> List(reddit_types.Post) {
  case sorted_acc {
    [] -> [p]
    [h, ..t] -> {
      case h {
        reddit_types.Post(_, _, _, _, _, score_h, _, _) -> {
          case p {
            reddit_types.Post(_, _, _, _, _, score_p, _, _) -> {
              case score_p >= score_h {
                True -> list.append([p], sorted_acc)
                False -> list.append([h], insert_sorted_by_score(t, p))
              }
            }
          }
        }
      }
    }
  }
}

// Pick a post for reposting (deterministic selection to avoid duplicates)
fn pick_post_for_repost(
posts: List(reddit_types.Post),
user_subreddit: String
) -> reddit_types.Post {
  // Filter out posts from the user's own subreddit (don't repost to same sub)
  let eligible = list.filter(posts, fn(p) {
    case p {
      reddit_types.Post(_, _, subreddit, _, _, _, _, _) -> subreddit != user_subreddit
    }
  })

  // If no eligible posts, just use first post
  case eligible {
    [] -> case posts {
      [first, ..] -> first
      [] -> reddit_types.Post(0, "deleted", "r/deleted", "Deleted", "Deleted", 0, list.new(), 0)
    }
    [first, ..] -> first
  }
}

// Helper functions
fn find_subreddit_index(subs: List(String), target: String) -> Int {
  find_subreddit_index_helper(subs, target, 1)
}

fn find_subreddit_index_helper(subs: List(String), target: String, idx: Int) -> Int {
  case subs {
    [] -> 1
    [h, ..t] -> case h == target {
      True -> idx
      False -> find_subreddit_index_helper(t, target, idx + 1)
    }
  }
}

fn calculate_post_frequency(sub_idx: Int, zipf_s: Int, sub_count: Int) -> Int {
  // Zipf: more popular subs (lower index) get more posts
  let base_posts = 3
  let zipf_factor = zipf.zipf_probability(sub_idx, zipf_s, sub_count)
  let frequency = base_posts + float_to_int(zipf_factor *. 10.0)
  frequency
}

@external(erlang, "erlang", "trunc")
fn float_to_int(f: Float) -> Int

fn create_multiple_posts(
state: reddit_engine.EngineState,
user: String,
subreddit: String,
count: Int,
last_post_id: Int,
metrics: List(reddit_metrics.Metric)
) -> #(reddit_engine.EngineState, Int, List(reddit_metrics.Metric)) {
  case count <= 0 {
    True -> #(state, last_post_id, metrics)
    False -> {
      let timestamp = now_ms()
      let new_metrics = list.append(metrics, [reddit_metrics.Metric("CreatePost", timestamp)])
      let result = reddit_engine.handle_message(
      state,
      reddit_types.CreatePost(user, subreddit, "Post " <> int.to_string(count), "Content")
      )
      case result {
        reddit_engine.EngineResult(new_state, _) -> {
          case new_state {
            reddit_engine.EngineState(_, _, _, _, new_post_id, _, _) ->
            create_multiple_posts(new_state, user, subreddit, count - 1, new_post_id, new_metrics)
          }
        }
      }
    }
  }
}

fn update_user_in_list(users: List(SimulatorUser), updated_user: SimulatorUser) -> List(SimulatorUser) {
  list.map(users, fn(u) {
    case u {
      SimulatorUser(name, _, _, _, _) -> case updated_user {
        SimulatorUser(updated_name, _, _, _, _) -> case name == updated_name {
          True -> updated_user
          False -> u
        }
      }
    }
  })
}

fn print_subreddit_distribution(state: reddit_engine.EngineState, subs: List(String)) {
  io.println("\n┌─ Subreddit Distribution (Zipf) ───────────────────────────┐")

  list.each(list.take(subs, 10), fn(sub) {
    let members = count_subreddit_members(state.subreddits, sub)
    let posts = count_subreddit_posts(state.global_posts, sub)
    io.println("│ " <> pad_right(sub, 15) <> " Members: " <> pad_right(int.to_string(members), 5) <> " Posts: " <> pad_right(int.to_string(posts), 5) <> " │")
  })

  io.println("└────────────────────────────────────────────────────────────┘")
}

fn count_subreddit_members(subs: List(reddit_engine.SubredditEntry), target: String) -> Int {
  list.fold(subs, 0, fn(acc, entry) {
    case entry {
      reddit_engine.SubredditEntry(name, subreddit) -> case name == target {
        True -> list.length(subreddit.members)
        False -> acc
      }
    }
  })
}

fn count_subreddit_posts(posts: List(reddit_types.Post), target: String) -> Int {
  list.fold(posts, 0, fn(acc, post) {
    case post {
      reddit_types.Post(_, _, subreddit, _, _, _, _, _) -> case subreddit == target {
        True -> acc + 1
        False -> acc
      }
    }
  })
}

fn print_user_activity(users: List(SimulatorUser)) {
  io.println("\n┌─ User Activity Statistics ────────────────────────────────┐")

  let total_posts = list.fold(users, 0, fn(acc, user) {
    case user {
      SimulatorUser(_, _, _, post_count, _) -> acc + post_count
    }
  })

  let avg_posts = case list.length(users) {
    0 -> 0.0
    n -> int.to_float(total_posts) /. int.to_float(n)
  }

  io.println("│ Total users:           " <> pad_right(int.to_string(list.length(users)), 31) <> "│")
  io.println("│ Total posts by users:  " <> pad_right(int.to_string(total_posts), 31) <> "│")
  io.println("│ Average posts/user:    " <> pad_right(float.to_string(avg_posts), 31) <> "│")
  io.println("└────────────────────────────────────────────────────────────┘")
}

fn pad_right(s: String, width: Int) -> String {
  let len = string.length(s)
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