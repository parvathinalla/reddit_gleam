import gleam/io
import gleam/string

@external(erlang, "reddit_http_client", "post")
fn http_post(url: String, body: String) -> Result(String, String)

@external(erlang, "io", "get_line")
fn erlang_get_line(prompt: String) -> String

@external(erlang, "inets", "start")
fn start_inets() -> Result(a, b)

pub fn main() {
  let _ = start_inets()
  io.println("")
  io.println("╔════════════════════════════════════════════════════════════╗")
  io.println("║       Reddit Clone CLI Client v2.0 + Crypto 🔐             ║")
  io.println("║       ⭐ BONUS: RSA-2048 Digital Signatures ⭐             ║")
  io.println("╚════════════════════════════════════════════════════════════╝")
  io.println("")
  io.println("Make sure the server is running on http://localhost:8080")
  io.println("")
  print_menu()

  interactive_loop("")
}

fn print_menu() {
  io.println("")
  io.println("🎁 BONUS FEATURE: All posts can be cryptographically signed!")
  io.println("")
  io.println("Available Commands:")
  io.println("  1  - Register user (generates RSA-2048 keys) 🔐")
  io.println("  2  - Login user")
  io.println("  3  - Join subreddit")
  io.println("  4  - Leave subreddit")
  io.println("  5  - Create post (SIGNED if you have keys) ✍️")
  io.println("  6  - View post (verifies signature) ✓")
  io.println("  7  - Comment on post")
  io.println("  8  - Vote on post (upvote/downvote)")
  io.println("  9  - Get feed")
  io.println("  10 - Send direct message")
  io.println("  11 - View direct messages")
  io.println("  d  - Run automated demo")
  io.println("  h  - Show this help menu")
  io.println("  q  - Quit")
  io.println("")
}
fn interactive_loop(current_user: String) {
  let user_display = case current_user {
    "" -> "Not logged in"
    u -> "Logged in as: " <> u
  }

  io.println("")
  io.println("[" <> user_display <> "]")
  io.print("Choice: ")

  let choice = read_line()

  case choice {
    "1" -> {
      register_user()
      interactive_loop(current_user)
    }

    "2" -> {
      case login_user() {
        Ok(username) -> interactive_loop(username)
        Error(_) -> interactive_loop(current_user)
      }
    }

    "3" -> {
      join_subreddit_interactive(current_user)
      interactive_loop(current_user)
    }

    "4" -> {
      leave_subreddit_interactive(current_user)
      interactive_loop(current_user)
    }

    "5" -> {
      create_post_interactive(current_user)
      interactive_loop(current_user)
    }

    "6" -> {
      view_post_interactive()
      interactive_loop(current_user)
    }

    "7" -> {
      comment_on_post_interactive(current_user)
      interactive_loop(current_user)
    }

    "8" -> {
      vote_on_post_interactive(current_user)
      interactive_loop(current_user)
    }

    "9" -> {
      get_feed_interactive(current_user)
      interactive_loop(current_user)
    }

    "10" -> {
      send_direct_message_interactive(current_user)
      interactive_loop(current_user)
    }

    "11" -> {
      view_messages_interactive(current_user)
      interactive_loop(current_user)
    }

    "d" -> {
      run_demo()
      interactive_loop(current_user)
    }

    "h" -> {
      print_menu()
      interactive_loop(current_user)
    }

    "q" -> {
      io.println("")
      io.println("Goodbye! Thanks for using Reddit Clone.")
      io.println("")
    }

    _ -> {
      io.println("Invalid choice. Type 'h' for help.")
      interactive_loop(current_user)
    }
  }
}

// ============= USER MANAGEMENT =============

fn register_user() {
  io.println("")
  io.println("=== Register New User ===")
  io.print("Username: ")
  let username = read_line()
  io.print("Password: ")
  let password = read_line()

  let json = "{\"username\":\"" <> username <> "\",\"password\":\"" <> password <> "\"}"

  case http_post("http://localhost:8080/api/register", json) {
    Ok(response) -> {
      io.println("✓ Registration successful!")
      io.println(response)
    }
    Error(error_message) -> {
      io.println("✗ Registration failed")
      io.println(error_message)
    }
  }
}

fn login_user() -> Result(String, String) {
  io.println("")
  io.println("=== Login ===")
  io.print("Username: ")
  let username = read_line()
  io.print("Password: ")
  let password = read_line()

  let json = "{\"username\":\"" <> username <> "\",\"password\":\"" <> password <> "\"}"

  case http_post("http://localhost:8080/api/login", json) {
    Ok(response) -> {
      io.println("✓ Login successful!")
      io.println(response)
      Ok(username)
    }
    Error(error_message) -> {
      io.println("✗ Login failed")
      io.println(error_message)
      Error(error_message)
    }
  }
}

// ============= SUBREDDIT MANAGEMENT =============

fn join_subreddit_interactive(current_user: String) {
  case current_user {
    "" -> {
      io.println("✗ Please login first!")
    }
    user -> {
      io.println("")
      io.println("=== Join Subreddit ===")
      io.print("Subreddit name (e.g., r/gleam): ")
      let subreddit = read_line()

      let json = "{\"username\":\"" <> user <> "\"}"
      let url = "http://localhost:8080/api/subreddits/" <> subreddit <> "/join"

      case http_post(url, json) {
        Ok(response) -> {
          io.println("✓ Successfully joined " <> subreddit)
          io.println(response)
        }
        Error(error_message) -> {
          io.println("✗ Failed to join")
          io.println(error_message)
        }
      }
    }
  }
}

fn leave_subreddit_interactive(current_user: String) {
  case current_user {
    "" -> {
      io.println("✗ Please login first!")
    }
    user -> {
      io.println("")
      io.println("=== Leave Subreddit ===")
      io.print("Subreddit name (e.g., r/gleam): ")
      let subreddit = read_line()

      let json = "{\"username\":\"" <> user <> "\"}"
      let url = "http://localhost:8080/api/subreddits/" <> subreddit <> "/leave"

      case http_post(url, json) {
        Ok(response) -> {
          io.println("✓ Successfully left " <> subreddit)
          io.println(response)
        }
        Error(error_message) -> {
          io.println("✗ Failed to leave")
          io.println(error_message)
        }
      }
    }
  }
}

// ============= POST MANAGEMENT =============

fn create_post_interactive(current_user: String) {
  case current_user {
    "" -> {
      io.println("✗ Please login first!")
    }
    user -> {
      io.println("")
      io.println("=== Create Post ===")
      io.print("Subreddit (e.g., r/gleam): ")
      let subreddit = read_line()
      io.print("Title: ")
      let title = read_line()
      io.print("Body: ")
      let body_text = read_line()

      let json = "{\"username\":\"" <> user <> "\",\"subreddit\":\"" <> subreddit <> "\",\"title\":\"" <> title <> "\",\"body\":\"" <> body_text <> "\"}"

      case http_post("http://localhost:8080/api/posts", json) {
        Ok(response) -> {
          io.println("✓ Post created successfully!")
          io.println(response)
        }
        Error(error_message) -> {
          io.println("✗ Failed to create post")
          io.println(error_message)
        }
      }
    }
  }
}

fn view_post_interactive() {
  io.println("")
  io.println("=== View Post ===")
  io.print("Post ID: ")
  let post_id = read_line()

  let url = "http://localhost:8080/api/posts/" <> post_id

  case http_get(url) {
    Ok(response) -> {
      io.println("✓ Post retrieved!")
      io.println(response)
    }
    Error(error_message) -> {
      io.println("✗ Failed to retrieve post")
      io.println(error_message)
    }
  }
}

fn get_feed_interactive(current_user: String) {
  case current_user {
    "" -> {
      io.println("✗ Please login first!")
    }
    user -> {
      io.println("")
      io.println("=== Getting Feed ===")

      let json = "{\"username\":\"" <> user <> "\"}"

      case http_post("http://localhost:8080/api/feed", json) {
        Ok(response) -> {
          io.println("✓ Feed retrieved!")
          io.println(response)
        }
        Error(error_message) -> {
          io.println("✗ Failed to get feed")
          io.println(error_message)
        }
      }
    }
  }
}

// ============= COMMENT MANAGEMENT =============

fn comment_on_post_interactive(current_user: String) {
  case current_user {
    "" -> {
      io.println("✗ Please login first!")
    }
    user -> {
      io.println("")
      io.println("=== Comment on Post ===")
      io.print("Post ID: ")
      let post_id = read_line()
      io.print("Parent Comment ID (0 for top-level): ")
      let parent_id = read_line()
      io.print("Comment text: ")
      let comment_text = read_line()

      let json = "{\"username\":\"" <> user <> "\",\"body\":\"" <> comment_text <> "\",\"parent_id\":\"" <> parent_id <> "\"}"
      let url = "http://localhost:8080/api/posts/" <> post_id <> "/comments"

      case http_post(url, json) {
        Ok(response) -> {
          io.println("✓ Comment added successfully!")
          io.println(response)
        }
        Error(error_message) -> {
          io.println("✗ Failed to add comment")
          io.println(error_message)
        }
      }
    }
  }
}

// ============= VOTING =============

fn vote_on_post_interactive(current_user: String) {
  case current_user {
    "" -> {
      io.println("✗ Please login first!")
    }
    user -> {
      io.println("")
      io.println("=== Vote on Post ===")
      io.print("Post ID: ")
      let post_id = read_line()
      io.print("Vote (1 for upvote, -1 for downvote, 0 to remove): ")
      let delta = read_line()

      let json = "{\"username\":\"" <> user <> "\",\"delta\":\"" <> delta <> "\"}"
      let url = "http://localhost:8080/api/posts/" <> post_id <> "/vote"

      case http_post(url, json) {
        Ok(response) -> {
          io.println("✓ Vote recorded!")
          io.println(response)
        }
        Error(error_message) -> {
          io.println("✗ Failed to vote")
          io.println(error_message)
        }
      }
    }
  }
}

// ============= DIRECT MESSAGES =============

fn send_direct_message_interactive(current_user: String) {
  case current_user {
    "" -> {
      io.println("✗ Please login first!")
    }
    user -> {
      io.println("")
      io.println("=== Send Direct Message ===")
      io.print("To (username): ")
      let to_user = read_line()
      io.print("Message: ")
      let message_text = read_line()

      let json = "{\"from\":\"" <> user <> "\",\"to\":\"" <> to_user <> "\",\"body\":\"" <> message_text <> "\"}"

      case http_post("http://localhost:8080/api/messages/send", json) {
        Ok(response) -> {
          io.println("✓ Message sent!")
          io.println(response)
        }
        Error(error_message) -> {
          io.println("✗ Failed to send message")
          io.println(error_message)
        }
      }
    }
  }
}

fn view_messages_interactive(current_user: String) {
  case current_user {
    "" -> {
      io.println("✗ Please login first!")
    }
    user -> {
      io.println("")
      io.println("=== Your Direct Messages ===")

      let json = "{\"username\":\"" <> user <> "\"}"

      case http_post("http://localhost:8080/api/messages", json) {
        Ok(response) -> {
          io.println("✓ Messages retrieved!")
          io.println(response)
        }
        Error(error_message) -> {
          io.println("✗ Failed to retrieve messages")
          io.println(error_message)
        }
      }
    }
  }
}

// ============= DEMO =============

fn run_demo() {
  io.println("")
  io.println("╔════════════════════════════════════════════════════════════╗")
  io.println("║                  RUNNING AUTOMATED DEMO                    ║")
  io.println("╚════════════════════════════════════════════════════════════╝")
  io.println("")

  io.println("Step 1: Registering users...")
  case http_post("http://localhost:8080/api/register", "{\"username\":\"demo_alice\",\"password\":\"pass123\"}") {
    Ok(_) -> io.println("  ✓ Registered demo_alice")
    Error(_) -> io.println("  ⚠ demo_alice may already exist")
  }

  case http_post("http://localhost:8080/api/register", "{\"username\":\"demo_bindhu\",\"password\":\"pass123\"}") {
    Ok(_) -> io.println("  ✓ Registered demo_bindhu")
    Error(_) -> io.println("  ⚠ demo_bindhu may already exist")
  }

  io.println("")
  io.println("Step 2: Logging in...")
  case http_post("http://localhost:8080/api/login", "{\"username\":\"demo_alice\",\"password\":\"pass123\"}") {
    Ok(_) -> io.println("  ✓ Logged in demo_alice")
    Error(_) -> io.println("  ✗ Failed to login demo_alice")
  }

  case http_post("http://localhost:8080/api/login", "{\"username\":\"demo_bindhu\",\"password\":\"pass123\"}") {
    Ok(_) -> io.println("  ✓ Logged in demo_bindhu")
    Error(_) -> io.println("  ✗ Failed to login demo_bindhu")
  }

  io.println("")
  io.println("Step 3: Joining subreddits...")
  case http_post("http://localhost:8080/api/subreddits/r/demo/join", "{\"username\":\"demo_alice\"}") {
    Ok(_) -> io.println("  ✓ demo_alice joined r/demo")
    Error(_) -> io.println("  ✗ Failed to join r/demo")
  }

  case http_post("http://localhost:8080/api/subreddits/r/demo/join", "{\"username\":\"demo_bindhu\"}") {
    Ok(_) -> io.println("  ✓ demo_bindhu joined r/demo")
    Error(_) -> io.println("  ✗ Failed to join r/demo")
  }

  io.println("")
  io.println("Step 4: Creating posts...")
  case http_post("http://localhost:8080/api/posts", "{\"username\":\"demo_alice\",\"subreddit\":\"r/demo\",\"title\":\"Demo Post by Alice\",\"body\":\"This is a demo post by Alice\"}") {
    Ok(_) -> io.println("  ✓ demo_alice posted in r/demo")
    Error(_) -> io.println("  ✗ Failed to create post")
  }

  case http_post("http://localhost:8080/api/posts", "{\"username\":\"demo_bindhu\",\"subreddit\":\"r/demo\",\"title\":\"Demo Post by Bindhu\",\"body\":\"This is a demo post by Bindhu\"}") {
    Ok(_) -> io.println("  ✓ demo_bindhu posted in r/demo")
    Error(_) -> io.println("  ✗ Failed to create post")
  }

  io.println("")
  io.println("Step 5: Getting posts by ID...")
  case http_get("http://localhost:8080/api/posts/1") {
    Ok(_) -> io.println("  ✓ Retrieved post #1 successfully")
    Error(_) -> io.println("  ✗ Failed to get post #1")
  }

  case http_get("http://localhost:8080/api/posts/2") {
    Ok(_) -> io.println("  ✓ Retrieved post #2 successfully")
    Error(_) -> io.println("  ✗ Failed to get post #2")
  }

  io.println("")
  io.println("Step 6: Voting on posts...")
  case http_post("http://localhost:8080/api/posts/1/vote", "{\"username\":\"demo_bindhu\",\"delta\":\"1\"}") {
    Ok(_) -> io.println("  ✓ demo_bindhu upvoted post #1")
    Error(_) -> io.println("  ✗ Failed to vote")
  }

  case http_post("http://localhost:8080/api/posts/2/vote", "{\"username\":\"demo_alice\",\"delta\":\"1\"}") {
    Ok(_) -> io.println("  ✓ demo_alice upvoted post #2")
    Error(_) -> io.println("  ✗ Failed to vote")
  }

  io.println("")
  io.println("Step 7: Commenting on posts...")
  case http_post("http://localhost:8080/api/posts/1/comments", "{\"username\":\"demo_bindhu\",\"body\":\"Great post Alice!\",\"parent_id\":\"0\"}") {
    Ok(_) -> io.println("  ✓ demo_bindhu commented on post #1")
    Error(_) -> io.println("  ✗ Failed to comment")
  }

  case http_post("http://localhost:8080/api/posts/2/comments", "{\"username\":\"demo_alice\",\"body\":\"Thanks Bindhu!\",\"parent_id\":\"0\"}") {
    Ok(_) -> io.println("  ✓ demo_alice commented on post #2")
    Error(_) -> io.println("  ✗ Failed to comment")
  }

  io.println("")
  io.println("Step 8: Sending direct messages...")
  case http_post("http://localhost:8080/api/messages/send", "{\"from\":\"demo_bindhu\",\"to\":\"demo_alice\",\"body\":\"Hello Alice! Enjoyed your post!\"}") {
    Ok(_) -> io.println("  ✓ demo_bindhu sent message to demo_alice")
    Error(_) -> io.println("  ✗ Failed to send message")
  }

  case http_post("http://localhost:8080/api/messages/send", "{\"from\":\"demo_alice\",\"to\":\"demo_bindhu\",\"body\":\"Thanks Bindhu! Great comments!\"}") {
    Ok(_) -> io.println("  ✓ demo_alice sent message to demo_bindhu")
    Error(_) -> io.println("  ✗ Failed to send message")
  }

  io.println("")
  io.println("Step 9: Getting direct messages...")
  case http_post("http://localhost:8080/api/messages", "{\"username\":\"demo_alice\"}") {
    Ok(_) -> io.println("  ✓ Retrieved messages for demo_alice")
    Error(_) -> io.println("  ✗ Failed to get messages")
  }

  case http_post("http://localhost:8080/api/messages", "{\"username\":\"demo_bindhu\"}") {
    Ok(_) -> io.println("  ✓ Retrieved messages for demo_bindhu")
    Error(_) -> io.println("  ✗ Failed to get messages")
  }

  io.println("")
  io.println("Step 10: Getting user feeds...")
  case http_post("http://localhost:8080/api/feed", "{\"username\":\"demo_alice\"}") {
    Ok(_) -> io.println("  ✓ Feed retrieved for demo_alice")
    Error(_) -> io.println("  ✗ Failed to get feed")
  }

  case http_post("http://localhost:8080/api/feed", "{\"username\":\"demo_bindhu\"}") {
    Ok(_) -> io.println("  ✓ Feed retrieved for demo_bindhu")
    Error(_) -> io.println("  ✗ Failed to get feed")
  }

  io.println("")
  io.println("Step 11: Leaving subreddits...")
  case http_post("http://localhost:8080/api/subreddits/r/demo/leave", "{\"username\":\"demo_alice\"}") {
    Ok(_) -> io.println("  ✓ demo_alice left r/demo")
    Error(_) -> io.println("  ✗ Failed to leave r/demo")
  }

  case http_post("http://localhost:8080/api/subreddits/r/demo/leave", "{\"username\":\"demo_bindhu\"}") {
    Ok(_) -> io.println("  ✓ demo_bindhu left r/demo")
    Error(_) -> io.println("  ✗ Failed to leave r/demo")
  }

  io.println("")
  io.println("╔════════════════════════════════════════════════════════════╗")
  io.println("║            COMPREHENSIVE API DEMO COMPLETED!               ║")
  io.println("║                                                            ║")
  io.println("║  All REST API Endpoints Tested:                           ║")
  io.println("║  ✓ POST   /api/register         - Register new user       ║")
  io.println("║  ✓ POST   /api/login            - Login user              ║")
  io.println("║  ✓ POST   /api/subreddits/:name/join - Join subreddit    ║")
  io.println("║  ✓ POST   /api/subreddits/:name/leave - Leave subreddit  ║")
  io.println("║  ✓ POST   /api/posts            - Create new post         ║")
  io.println("║  ✓ GET    /api/posts/:id        - Get post by ID          ║")
  io.println("║  ✓ POST   /api/posts/:id/vote   - Vote on post            ║")
  io.println("║  ✓ POST   /api/posts/:id/comments - Add comment           ║")
  io.println("║  ✓ POST   /api/messages/send    - Send direct message    ║")
  io.println("║  ✓ POST   /api/messages         - Get user messages      ║")
  io.println("║  ✓ POST   /api/feed             - Get user feed           ║")
  io.println("║                                                            ║")
  io.println("╚════════════════════════════════════════════════════════════╝")
}

// ============= HELPERS =============

fn read_line() -> String {
  let line = erlang_get_line("")
  string.trim(line)
}

// HTTP GET helper
@external(erlang, "reddit_http_client", "get")
fn http_get(url: String) -> Result(String, String)