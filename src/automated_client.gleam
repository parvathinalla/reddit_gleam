import gleam/io
import gleam/string
import gleam/int
import gleam/list

@external(erlang, "reddit_http_client", "post")
fn http_post(url: String, body: String) -> Result(String, String)

@external(erlang, "reddit_http_client", "get")
fn http_get(url: String) -> Result(String, String)

@external(erlang, "inets", "start")
fn start_inets() -> Result(a, b)

@external(erlang, "timer", "sleep")
fn sleep(milliseconds: Int) -> Nil

@external(erlang, "erlang", "system_time")
fn system_time_internal(unit: atom) -> Int

@external(erlang, "init", "get_plain_arguments")
fn erlang_get_args() -> List(String)

@external(erlang, "rand", "uniform")
fn random_uniform(n: Int) -> Int

// Helper to create atom for time unit
fn millisecond_atom() -> atom {
  atom_from_string("millisecond")
}

@external(erlang, "erlang", "binary_to_atom")
fn atom_from_string(s: String) -> atom

fn system_time_ms() -> Int {
  system_time_internal(millisecond_atom())
}

pub fn main() {
  let _ = start_inets()

  // Get command line arguments
  let args = erlang_get_args()

  case args {
    [username, action_count_str] -> {
      case int.parse(action_count_str) {
        Ok(count) -> {
          run_automated_session(username, count)
        }
        Error(_) -> {
          print_usage()
        }
      }
    }
    [username] -> {
      // Default to 10 actions if count not specified
      run_automated_session(username, 10)
    }
    _ -> {
      print_usage()
    }
  }
}

fn print_usage() {
  io.println("")
  io.println("Usage: gleam run -m automated_client <username> [action_count]")
  io.println("")
  io.println("Arguments:")
  io.println("  username      - Username for the automated client session")
  io.println("  action_count  - Number of actions to perform (default: 10)")
  io.println("")
  io.println("Examples:")
  io.println("  gleam run -m automated_client alice 15")
  io.println("  gleam run -m automated_client bindhu")
  io.println("")
}

fn run_automated_session(username: String, action_count: Int) {
  let start_time = system_time_ms()

  print_header(username, action_count)

  // Phase 1: Account Setup
  log_phase(username, "PHASE 1: Account Setup")

  case register_user(username, "password123") {
    Ok(_) -> log_success(username, "Registered successfully")
    Error(msg) -> log_warning(username, "Registration: " <> msg <> " (may already exist)")
  }
  sleep(random_delay())

  // Try to login - if it fails, stop the session
  let login_result = login_user(username, "password123")
  case login_result {
    Error(msg) -> {
      log_error(username, "Login failed: " <> msg)
      log_error(username, "Cannot continue without login. Exiting.")
    }
    Ok(_) -> {
      log_success(username, "Logged in successfully")
      sleep(random_delay())

      // Phase 2: Join Subreddits
      log_phase(username, "PHASE 2: Joining Subreddits")
      join_subreddits(username)
      sleep(random_delay())

      // Phase 3: Create Posts
      log_phase(username, "PHASE 3: Creating Posts")
      let post_count = int.max(1, action_count / 3)
      create_posts(username, post_count)
      sleep(random_delay())

      // Phase 4: Interaction (Vote & Comment)
      log_phase(username, "PHASE 4: User Interactions")
      interact_with_posts(username, action_count)
      sleep(random_delay())

      // Phase 5: View Feed
      log_phase(username, "PHASE 5: Viewing Feed")
      case get_feed(username) {
        Ok(response) -> {
          log_success(username, "Feed retrieved successfully")
          let preview = string.slice(response, 0, 100)
          log_info(username, "Feed preview: " <> preview <> "...")
        }
        Error(msg) -> log_error(username, "Failed to get feed: " <> msg)
      }
      sleep(random_delay())

      // Phase 6: Direct Messages
      log_phase(username, "PHASE 6: Direct Messaging")
      send_direct_messages(username)
      sleep(random_delay())

      case get_messages(username) {
        Ok(_) -> log_success(username, "Checked inbox")
        Error(msg) -> log_error(username, "Failed to check inbox: " <> msg)
      }

      // Phase 7: View Specific Post
      log_phase(username, "PHASE 7: Viewing Post Details")
      case get_post(1) {
        Ok(_) -> log_success(username, "Retrieved post #1 with comments")
        Error(msg) -> log_warning(username, "Failed to get post: " <> msg)
      }

      let end_time = system_time_ms()
      let duration_ms = end_time - start_time
      let duration_sec = duration_ms / 1000

      print_footer(username, action_count, duration_sec)
    }
  }
}


// ============================================================================
// Phase Implementations
// ============================================================================

fn join_subreddits(username: String) {
  let subreddits = ["r/gleam", "r/erlang", "r/beam", "r/functional"]

  list.each(subreddits, fn(subreddit) {
    log_action(username, "Joining " <> subreddit <> "...")
    case join_subreddit(username, subreddit) {
      Ok(_) -> {
        log_success(username, "Joined " <> subreddit)
      }
      Error(msg) -> {
        log_warning(username, "Join " <> subreddit <> ": " <> msg)
      }
    }
    sleep(random_delay_short())
  })
}

fn create_posts(username: String, count: Int) {
  create_posts_helper(username, count, 1)
}

fn create_posts_helper(username: String, count: Int, current: Int) {
  case current > count {
    True -> Nil
    False -> {
      let subreddit = select_random_subreddit(current)
      let title = "Post #" <> int.to_string(current) <> " by " <> username
      let body = "This is automated post number " <> int.to_string(current) <>
      " created by " <> username <> " in " <> subreddit <>
      ". Testing concurrent Reddit Clone functionality!"

      log_action(username, "Creating post in " <> subreddit <> "...")

      case create_post(username, subreddit, title, body) {
        Ok(response) -> {
          log_success(username, "Post #" <> int.to_string(current) <> " created")

          // Try to extract post ID from response
          let _ = case string.contains(response, "post_id") {
            True -> log_info(username, "Response: " <> response)
            False -> Nil
          }
        }
        Error(msg) -> {
          log_error(username, "Failed to create post: " <> msg)
        }
      }

      sleep(random_delay())
      create_posts_helper(username, count, current + 1)
    }
  }
}

fn interact_with_posts(username: String, action_count: Int) {
  let interaction_count = int.max(2, action_count / 2)

  // Vote on posts
  log_action(username, "Voting on posts...")
  vote_on_posts(username, interaction_count)

  sleep(random_delay())

  // Comment on posts
  log_action(username, "Adding comments...")
  comment_on_posts(username, int.max(1, interaction_count / 2))
}

fn vote_on_posts(username: String, count: Int) {
  vote_on_posts_helper(username, count, 1)
}

fn vote_on_posts_helper(username: String, count: Int, current: Int) {
  case current > count {
    True -> Nil
    False -> {
      let post_id = current
      let delta = case random_uniform(10) {
        n if n > 3 -> 1  // 70% upvotes
        _ -> -1          // 30% downvotes
      }

      let vote_type = case delta {
        1 -> "Upvoting"
        _ -> "Downvoting"
      }

      case vote_post(username, post_id, delta) {
        Ok(_) -> {
          log_success(username, vote_type <> " post #" <> int.to_string(post_id))
        }
        Error(_) -> {
          // Silent failure for voting (post might not exist)
          Nil
        }
      }

      sleep(random_delay_short())
      vote_on_posts_helper(username, count, current + 1)
    }
  }
}

fn comment_on_posts(username: String, count: Int) {
  comment_on_posts_helper(username, count, 1)
}

fn comment_on_posts_helper(username: String, count: Int, current: Int) {
  case current > count {
    True -> Nil
    False -> {
      let post_id = current
      let comment_body = "Interesting post! Comment #" <> int.to_string(current) <>
      " from " <> username <> ". Really enjoying this discussion!"

      case create_comment(username, post_id, 0, comment_body) {
        Ok(response) -> {
          log_success(username, "Comment added to post #" <> int.to_string(post_id))

          // Check if we got a comment ID back
          let _ = case string.contains(response, "comment_id") {
            True -> log_info(username, "Response: " <> response)
            False -> Nil
          }
        }
        Error(_) -> {
          // Silent failure (post might not exist)
          Nil
        }
      }

      sleep(random_delay())
      comment_on_posts_helper(username, count, current + 1)
    }
  }
}

fn send_direct_messages(username: String) {
  let recipients = get_other_users(username)

  case recipients {
    [] -> {
      log_warning(username, "No other users to message")
    }
    [recipient, ..] -> {
      let message = "Hello " <> recipient <> "! This is an automated message from " <>
      username <> ". Hope you're having a great day!"

      log_action(username, "Sending DM to " <> recipient <> "...")
      case send_dm(username, recipient, message) {
        Ok(_) -> {
          log_success(username, "DM sent to " <> recipient)
        }
        Error(msg) -> {
          log_warning(username, "Failed to send DM: " <> msg)
        }
      }
    }
  }
}

fn register_user(username: String, password: String) -> Result(String, String) {
  let json = build_json([
  #("username", username),
  #("password", password)
  ])
  http_post("http://localhost:8080/api/register", json)
}

fn login_user(username: String, password: String) -> Result(String, String) {
  let json = build_json([
  #("username", username),
  #("password", password)
  ])
  http_post("http://localhost:8080/api/login", json)
}

fn join_subreddit(username: String, subreddit: String) -> Result(String, String) {
  let json = build_json([#("username", username)])
  let url = "http://localhost:8080/api/subreddits/" <> subreddit <> "/join"
  http_post(url, json)
}

fn create_post(username: String, subreddit: String, title: String, body: String) -> Result(String, String) {
  let json = build_json([
  #("username", username),
  #("subreddit", subreddit),
  #("title", title),
  #("body", body)
  ])
  http_post("http://localhost:8080/api/posts", json)
}

fn get_feed(username: String) -> Result(String, String) {
  let json = build_json([#("username", username)])
  http_post("http://localhost:8080/api/feed", json)
}

fn vote_post(username: String, post_id: Int, delta: Int) -> Result(String, String) {
  let json = build_json([
  #("username", username),
  #("delta", int.to_string(delta))
  ])
  let url = "http://localhost:8080/api/posts/" <> int.to_string(post_id) <> "/vote"
  http_post(url, json)
}

fn create_comment(username: String, post_id: Int, parent_id: Int, body: String) -> Result(String, String) {
  let json = build_json([
  #("username", username),
  #("body", body),
  #("parent_id", int.to_string(parent_id))
  ])
  let url = "http://localhost:8080/api/posts/" <> int.to_string(post_id) <> "/comments"
  http_post(url, json)
}

fn send_dm(from: String, to: String, body: String) -> Result(String, String) {
  let json = build_json([
  #("from", from),
  #("to", to),
  #("body", body)
  ])
  http_post("http://localhost:8080/api/messages/send", json)
}

fn get_messages(username: String) -> Result(String, String) {
  let json = build_json([#("username", username)])
  http_post("http://localhost:8080/api/messages", json)
}

fn get_post(post_id: Int) -> Result(String, String) {
  let url = "http://localhost:8080/api/posts/" <> int.to_string(post_id)
  http_get(url)
}

fn build_json(fields: List(#(String, String))) -> String {
  let pairs = list.map(fields, fn(field) {
    let #(key, value) = field
    "\"" <> key <> "\":\"" <> value <> "\""
  })
  "{" <> string.join(pairs, ",") <> "}"
}

fn select_random_subreddit(index: Int) -> String {
  let subreddits = ["r/gleam", "r/erlang", "r/beam", "r/functional"]
  let idx = index % list.length(subreddits)

  case list.drop(subreddits, idx) {
    [first, ..] -> first
    [] -> "r/gleam"
  }
}

fn get_other_users(current_user: String) -> List(String) {
  let all_users = ["alice", "bindhu", "charlie", "diana", "eve"]
  list.filter(all_users, fn(user) { user != current_user })
}

fn random_delay() -> Int {
  500 + random_uniform(1000)  // 500-1500ms
}

fn random_delay_short() -> Int {
  200 + random_uniform(500)   // 200-700ms
}

fn print_header(username: String, action_count: Int) {
  io.println("")
  io.println("╔════════════════════════════════════════════════════════════╗")
  let padding = string.repeat(" ", 40 - string.length(username))
  io.println("║  Automated Client: " <> username <> padding <> "║")
  io.println("║  Actions: " <> int.to_string(action_count) <> string.repeat(" ", 48 - string.length(int.to_string(action_count))) <> "║")
  io.println("╚════════════════════════════════════════════════════════════╝")
  io.println("")
}

fn print_footer(username: String, actions: Int, duration_sec: Int) {
  io.println("")
  io.println("╔════════════════════════════════════════════════════════════╗")
  io.println("║  " <> username <> " - Session Complete" <> string.repeat(" ", 32 - string.length(username)) <> "║")
  io.println("║  Total Actions: " <> int.to_string(actions) <> string.repeat(" ", 40 - string.length(int.to_string(actions))) <> "║")
  io.println("║  Duration: " <> int.to_string(duration_sec) <> " seconds" <> string.repeat(" ", 40 - string.length(int.to_string(duration_sec) <> " seconds")) <> "║")
  io.println("╚════════════════════════════════════════════════════════════╝")
  io.println("")
}

fn log_phase(username: String, phase: String) {
  io.println("")
  io.println("--- [" <> username <> "] " <> phase <> " ---")
}

fn log_action(username: String, message: String) {
  let timestamp = format_timestamp()
  io.println("[" <> timestamp <> "] [" <> username <> "] " <> message)
}

fn log_success(username: String, message: String) {
  let timestamp = format_timestamp()
  io.println("[" <> timestamp <> "] [" <> username <> "] ✓ " <> message)
}

fn log_error(username: String, message: String) {
  let timestamp = format_timestamp()
  io.println("[" <> timestamp <> "] [" <> username <> "] ✗ " <> message)
}

fn log_warning(username: String, message: String) {
  let timestamp = format_timestamp()
  io.println("[" <> timestamp <> "] [" <> username <> "] ⚠ " <> message)
}

fn log_info(username: String, message: String) {
  let timestamp = format_timestamp()
  io.println("[" <> timestamp <> "] [" <> username <> "] ℹ " <> message)
}

fn format_timestamp() -> String {
  let ms = system_time_ms()
  int.to_string(ms)
}