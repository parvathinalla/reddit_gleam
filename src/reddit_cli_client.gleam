import gleam/io
import gleam/string

@external(erlang, "reddit_http_client", "post")
fn http_post(url: String, body: String) -> Result(String, String)

@external(erlang, "io", "get_line")
fn erlang_get_line(prompt: String) -> String

@external(erlang, "inets", "start")
fn start_inets() -> Result(a, b)

pub fn main() {
  // Start inets (ignore result)
  let _ = start_inets()

  io.println("")
  io.println("╔════════════════════════════════════════════════════════════╗")
  io.println("║       Reddit Clone CLI Client v2.0                        ║")
  io.println("╚════════════════════════════════════════════════════════════╝")
  io.println("")
  io.println("Make sure the server is running on http://localhost:8080")
  io.println("")
  io.println("Commands:")
  io.println("  1 - Register user")
  io.println("  2 - Login user")
  io.println("  3 - Join subreddit")
  io.println("  4 - Create post")
  io.println("  5 - Get feed")
  io.println("  d - Run demo (automated)")
  io.println("  q - Quit")
  io.println("")

  interactive_loop("")
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
      create_post_interactive(current_user)
      interactive_loop(current_user)
    }

    "5" -> {
      get_feed_interactive(current_user)
      interactive_loop(current_user)
    }

    "d" -> {
      run_demo()
      interactive_loop(current_user)
    }

    "q" -> {
      io.println("")
      io.println("Goodbye! Thanks for using Reddit Clone.")
      io.println("")
    }

    _ -> {
      io.println("Invalid choice. Please try again.")
      interactive_loop(current_user)
    }
  }
}

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

fn run_demo() {
  io.println("")
  io.println("╔════════════════════════════════════════════════════════════╗")
  io.println("║                  RUNNING AUTOMATED DEMO                    ║")
  io.println("╚════════════════════════════════════════════════════════════╝")
  io.println("")

  io.println("Step 1: Registering users...")
  case http_post("http://localhost:8080/api/register", "{\"username\":\"alice\",\"password\":\"secret123\"}") {
    Ok(_) -> io.println("  ✓ Registered alice")
    Error(_) -> io.println("  ✗ Failed to register alice")
  }

  case http_post("http://localhost:8080/api/register", "{\"username\":\"bob\",\"password\":\"password456\"}") {
    Ok(_) -> io.println("  ✓ Registered bob")
    Error(_) -> io.println("  ✗ Failed to register bob")
  }

  io.println("")
  io.println("Step 2: Logging in...")
  case http_post("http://localhost:8080/api/login", "{\"username\":\"alice\",\"password\":\"secret123\"}") {
    Ok(_) -> io.println("  ✓ Logged in alice")
    Error(_) -> io.println("  ✗ Failed to login alice")
  }

  io.println("")
  io.println("Step 3: Joining subreddits...")
  case http_post("http://localhost:8080/api/subreddits/r/gleam/join", "{\"username\":\"alice\"}") {
    Ok(_) -> io.println("  ✓ alice joined r/gleam")
    Error(_) -> io.println("  ✗ Failed to join r/gleam")
  }

  io.println("")
  io.println("Step 4: Creating posts...")
  case http_post("http://localhost:8080/api/posts", "{\"username\":\"alice\",\"subreddit\":\"r/gleam\",\"title\":\"Hello\",\"body\":\"First post\"}") {
    Ok(_) -> io.println("  ✓ alice posted in r/gleam")
    Error(_) -> io.println("  ✗ Failed to create post")
  }

  io.println("")
  io.println("Step 5: Getting feed...")
  case http_post("http://localhost:8080/api/feed", "{\"username\":\"alice\"}") {
    Ok(response) -> {
      io.println("  ✓ Feed retrieved")
      io.println("  " <> response)
    }
    Error(_) -> io.println("  ✗ Failed to get feed")
  }

  io.println("")
  io.println("╔════════════════════════════════════════════════════════════╗")
  io.println("║                  DEMO COMPLETED!                           ║")
  io.println("╚════════════════════════════════════════════════════════════╝")
}

fn read_line() -> String {
  let line = erlang_get_line("")
  string.trim(line)
}