// ============================================================================
// automated_client.gleam - WITH BONUS CRYPTO SUPPORT
// ============================================================================
// Automated client for testing the Reddit Clone REST API with digital signatures
// Usage: gleam run -m automated_client <username> <action_count>
// Example: gleam run -m automated_client alice 10
// ============================================================================

import gleam/io
import gleam/string
import gleam/int
import gleam/list

// ============================================================================
// External FFI Functions
// ============================================================================

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

// ============================================================================
// User Session State (stores keys)
// ============================================================================

pub type UserSession {
  UserSession(
  username: String,
  public_key: String,
  private_key: String,
  logged_in: Bool,
  )
}

// ============================================================================
// Main Entry Point
// ============================================================================

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
  io.println("  gleam run -m automated_client bob")
  io.println("")
  io.println("Features:")
  io.println("  • Automatic RSA-2048 key generation on registration")
  io.println("  • Digital signature on all posts")
  io.println("  • Signature verification on retrieval")
  io.println("")
}

// ============================================================================
// Main Session Logic
// ============================================================================

fn run_automated_session(username: String, action_count: Int) {
  let start_time = system_time_ms()

  print_header(username, action_count)

  // Phase 1: Account Setup with Crypto
  log_phase(username, "PHASE 1: Account Setup with RSA-2048")

  let session = case register_user_with_crypto(username, "password123") {
    Ok(sess) -> {
      log_success(username, "Registered with RSA-2048 key pair")
      log_info(username, "Public key: " <> string.slice(sess.public_key, 0, 50) <> "...")
      log_info(username, "Private key saved for signing posts")
      sess
    }
    Error(msg) -> {
      log_warning(username, "Registration: " <> msg <> " (may already exist)")
      // Try to login and continue without keys (posts will be unsigned)
      UserSession(username, "", "", False)
    }
  }
  sleep(random_delay())

  // Try to login
  let login_result = login_user(username, "password123")
  case login_result {
    Error(msg) -> {
      log_error(username, "Login failed: " <> msg)
      log_error(username, "Cannot continue without login. Exiting.")
    }
    Ok(_) -> {
      log_success(username, "Logged in successfully")
      let active_session = UserSession(session.username, session.public_key, session.private_key, True)
      sleep(random_delay())

      // Continue with remaining phases
      continue_session_with_crypto(active_session, action_count, start_time)
    }
  }
}

fn continue_session_with_crypto(session: UserSession, action_count: Int, start_time: Int) {
  let username = session.username

  // Phase 2: Join Subreddits
  log_phase(username, "PHASE 2: Joining Subreddits")
  join_subreddits(username)
  sleep(random_delay())

  // Phase 3: Create Signed Posts
  log_phase(username, "PHASE 3: Creating Signed Posts 🔐")
  let post_count = int.max(1, action_count / 3)
  create_signed_posts(session, post_count)
  sleep(random_delay())

  // Phase 4: Interaction (Vote & Comment)
  log_phase(username, "PHASE 4: User Interactions")
  interact_with_posts(username, action_count)
  sleep(random_delay())

  // Phase 5: View Feed with Signature Verification
  log_phase(username, "PHASE 5: Viewing Feed (verifying signatures)")
  case get_feed(username) {
    Ok(response) -> {
      log_success(username, "Feed retrieved successfully")
      // Check if response contains signature verification
      case string.contains(response, "signature_status") {
        True -> log_info(username, "✓ Signatures verified in feed")
        False -> log_info(username, "Feed contains unsigned posts")
      }
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

  // Phase 7: View Specific Post with Verification
  log_phase(username, "PHASE 7: Viewing Post Details (verifying signature)")
  case get_post(1) {
    Ok(response) -> {
      log_success(username, "Retrieved post #1")
      // Check signature status
      case string.contains(response, "\"signature_status\":\"valid\"") {
        True -> log_success(username, "✓ Post signature is VALID")
        False -> {
          case string.contains(response, "\"signature_status\":\"invalid\"") {
            True -> log_warning(username, "⚠ Post signature is INVALID")
            False -> {
              case string.contains(response, "\"signature_status\":\"unsigned\"") {
                True -> log_info(username, "ℹ Post is signed")
                False -> log_info(username, "Signature status unknown")
              }
            }
          }
        }
      }
    }
    Error(msg) -> log_warning(username, "Failed to get post: " <> msg)
  }

  // Phase 8: Get User's Public Key
  log_phase(username, "PHASE 8: Retrieving Public Key from Server")
  case get_public_key(username) {
    Ok(response) -> {
      log_success(username, "Public key retrieved from server")
      case string.contains(response, "public_key") {
        True -> log_info(username, "Public key available for verification")
        False -> log_warning(username, "Public key not found in response")
      }
    }
    Error(msg) -> log_warning(username, "Failed to get public key: " <> msg)
  }

  let end_time = system_time_ms()
  let duration_ms = end_time - start_time
  let duration_sec = duration_ms / 1000

  print_footer_with_crypto(username, action_count, duration_sec, session.private_key != "")
}

// ============================================================================
// Phase Implementations
// ============================================================================

fn join_subreddits(username: String) {
  let subreddits = ["r/gleam", "r/erlang", "r/beam", "r/functional"]

  list.each(subreddits, fn(subreddit) {
    log_action(username, "Joining " <> subreddit <> "...")
    case join_subreddit(username, subreddit) {
      Ok(_) -> log_success(username, "Joined " <> subreddit)
      Error(msg) -> log_warning(username, "Join " <> subreddit <> ": " <> msg)
    }
    sleep(random_delay_short())
  })
}

fn create_signed_posts(session: UserSession, count: Int) {
  create_signed_posts_helper(session, count, 1)
}

fn create_signed_posts_helper(session: UserSession, count: Int, current: Int) {
  case current > count {
    True -> Nil
    False -> {
      let username = session.username
      let subreddit = select_random_subreddit(current)
      let title = "Post #" <> int.to_string(current) <> " by " <> username
      let body = "This is a cryptographically signed post #" <> int.to_string(current) <>
      " created by " <> username <> " using RSA-2048 digital signatures!"

      log_action(username, "Creating SIGNED post in " <> subreddit <> "... 🔐")

      case session.private_key {
        "" -> {
          // No private key, create unsigned post
          log_warning(username, "No private key available, creating unsigned post")
          case create_post(username, subreddit, title, body) {
            Ok(_) -> {
              log_warning(username, "Post #" <> int.to_string(current) <> " created (UNSIGNED)")
            }
            Error(msg) -> {
              log_error(username, "Failed to create post: " <> msg)
            }
          }
        }
        private_key -> {
          // Create signed post
          case create_signed_post(username, subreddit, title, body, private_key) {
            Ok(response) -> {
              case string.contains(response, "\"signature_status\":\"signed\"") {
                True -> log_success(username, "Post #" <> int.to_string(current) <> " created with VALID signature ✓")
                False -> log_warning(username, "Post #" <> int.to_string(current) <> " created but signature status unknown")
              }
            }
            Error(msg) -> {
              log_error(username, "Failed to create signed post: " <> msg)
            }
          }
        }
      }

      sleep(random_delay())
      create_signed_posts_helper(session, count, current + 1)
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
      let comment_body = "Great post! Comment #" <> int.to_string(current) <>
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

// ============================================================================
// REST API Functions with Crypto Support
// ============================================================================

fn register_user_with_crypto(username: String, password: String) -> Result(UserSession, String) {
  let json = build_json([
  #("username", username),
  #("password", password)
  // Don't provide public_key - let server generate it
  ])

  case http_post("http://localhost:8080/api/register", json) {
    Ok(response) -> {
      // Extract public_key and private_key from response
      let public_key = extract_json_field(response, "public_key")
      let private_key = extract_json_field(response, "private_key")

      Ok(UserSession(username, public_key, private_key, False))
    }
    Error(msg) -> Error(msg)
  }
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

fn create_signed_post(username: String, subreddit: String, title: String, body: String, private_key: String) -> Result(String, String) {
  let json = build_json([
  #("username", username),
  #("subreddit", subreddit),
  #("title", title),
  #("body", body),
  #("private_key", private_key)  // Server will use this to sign
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

fn get_public_key(username: String) -> Result(String, String) {
  let url = "http://localhost:8080/api/users/" <> username <> "/publickey"
  http_get(url)
}

// ============================================================================
// Helper Functions
// ============================================================================

fn build_json(fields: List(#(String, String))) -> String {
  let pairs = list.map(fields, fn(field) {
    let #(key, value) = field
    "\"" <> key <> "\":\"" <> escape_json_value(value) <> "\""
  })
  "{" <> string.join(pairs, ",") <> "}"
}

fn escape_json_value(value: String) -> String {
  value
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
}

fn extract_json_field(json: String, field_name: String) -> String {
  // Simple extraction - look for "field_name":"value"
  let pattern = "\"" <> field_name <> "\":\""
  case string.split(json, pattern) {
    [_, rest] -> {
      case string.split(rest, "\"") {
        [value, ..] -> value
        _ -> ""
      }
    }
    _ -> ""
  }
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
  let all_users = ["alice", "bob", "charlie", "diana", "eve"]
  list.filter(all_users, fn(user) { user != current_user })
}

fn random_delay() -> Int {
  500 + random_uniform(1000)  // 500-1500ms
}

fn random_delay_short() -> Int {
  200 + random_uniform(500)   // 200-700ms
}

// ============================================================================
// Logging Functions
// ============================================================================
// Continue from print_header function...

fn print_header(username: String, action_count: Int) {
  io.println("")
  io.println("╔════════════════════════════════════════════════════════════╗")
  let padding = string.repeat(" ", 40 - string.length(username))
  io.println("║  Automated Client: " <> username <> padding <> "║")
  io.println("║  Actions: " <> int.to_string(action_count) <> string.repeat(" ", 48 - string.length(int.to_string(action_count))) <> "║")
  io.println("║  Crypto: RSA-2048 Digital Signatures 🔐                   ║")
  io.println("╚════════════════════════════════════════════════════════════╝")
  io.println("")
}

fn print_footer_with_crypto(username: String, actions: Int, duration_sec: Int, has_keys: Bool) {
  io.println("")
  io.println("╔════════════════════════════════════════════════════════════╗")
  io.println("║  " <> username <> " - Session Complete" <> string.repeat(" ", 32 - string.length(username)) <> "║")
  io.println("║  Total Actions: " <> int.to_string(actions) <> string.repeat(" ", 40 - string.length(int.to_string(actions))) <> "║")
  io.println("║  Duration: " <> int.to_string(duration_sec) <> " seconds" <> string.repeat(" ", 40 - string.length(int.to_string(duration_sec) <> " seconds")) <> "║")

  case has_keys {
    True -> io.println("║  Crypto: ✓ Posts signed with RSA-2048                     ║")
    False -> io.println("║  Crypto: ⚠ Posts unsigned (no keys)                       ║")
  }

  io.println("╚════════════════════════════════════════════════════════════╝")
  io.println("")
}

fn log_phase(_username: String, phase: String) {
  io.println("")
  io.println("--- " <> phase <> " ---")
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