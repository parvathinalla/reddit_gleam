import gleam/list
import reddit_engine
import reddit_types

// Small helper to avoid literal-assert warnings: returns False but is a function call
fn unexpected(_s: String) -> Bool { False }

fn unwrap_state(res: reddit_engine.EngineResult) -> reddit_engine.EngineState {
  case res { reddit_engine.EngineResult(s, _) -> s }
}

// Test join/create subreddit and posting permissions (sequential style)
pub fn membership_join_and_post_test() {
  let state0 = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)

  let s1 = unwrap_state(reddit_engine.handle_message(state0, reddit_types.Join("alice")))
  let res = reddit_engine.handle_message(s1, reddit_types.JoinSub("alice", "r/foo"))
  case res {
    reddit_engine.EngineResult(s2, reddit_types.Ok) -> {
      // Ensure subreddit exists and alice is a member
      let is_member = list.any(s2.subreddits, fn(e) { case e { reddit_engine.SubredditEntry(n, sub) -> n == "r/foo" && list.any(sub.members, fn(m) { m == "alice" }) } })
      assert is_member

      // Creating a post should succeed
      let res_post = reddit_engine.handle_message(s2, reddit_types.CreatePost("alice", "r/foo", "Hi", "Body"))
      case res_post {
        reddit_engine.EngineResult(_, reddit_types.OkWithId(_)) -> { let _ = 0 }
        _ -> {
          assert unexpected("membership_join_and_post_test: create post")
          let _ = 0
        }
      }

      let _ = 0
    }
    _ -> {
      assert unexpected("membership_join_and_post_test: join sub")
      let _ = 0
    }
  }
}

// Test joining twice returns already_member error
pub fn membership_double_join_test() {
  let state0 = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)
  let s1 = unwrap_state(reddit_engine.handle_message(state0, reddit_types.Join("bob")))
  let s2 = unwrap_state(reddit_engine.handle_message(s1, reddit_types.JoinSub("bob", "r/bar")))

  let res = reddit_engine.handle_message(s2, reddit_types.JoinSub("bob", "r/bar"))
  case res {
    reddit_engine.EngineResult(_, reddit_types.Error(reason)) -> { assert reason == "already_member" }
    _ -> {
      assert unexpected("membership_double_join_test")
      Nil
    }
  }
}

// Test leaving and invalid leave behaviour
pub fn membership_leave_test() {
  let state0 = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)
  let s1 = unwrap_state(reddit_engine.handle_message(state0, reddit_types.Join("carol")))

  // Leaving a subreddit that doesn't exist should return subreddit_not_found
  let res1 = reddit_engine.handle_message(s1, reddit_types.LeaveSub("carol", "r/nope"))
  case res1 {
    reddit_engine.EngineResult(_, reddit_types.Error(reason)) -> { assert reason == "subreddit_not_found" }
    _ -> {
      assert unexpected("membership_leave_test no subreddit")
      Nil
    }
  }

  // Create subreddit by another user and attempt to leave when not member
  let s3 = unwrap_state(reddit_engine.handle_message(s1, reddit_types.Join("dave")))
  let s4 = unwrap_state(reddit_engine.handle_message(s3, reddit_types.JoinSub("dave", "r/baz")))

  let res2 = reddit_engine.handle_message(s4, reddit_types.LeaveSub("carol", "r/baz"))
  case res2 {
    reddit_engine.EngineResult(_, reddit_types.Error(reason2)) -> { assert reason2 == "not_member" }
    _ -> {
      assert unexpected("membership_leave_test not member")
      Nil
    }
  }
}

// Test creating a post as non-member (when subreddit exists) is rejected
pub fn create_post_non_member_test() {
  let state0 = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)
  let s1 = unwrap_state(reddit_engine.handle_message(state0, reddit_types.Join("erin")))
  let s2 = unwrap_state(reddit_engine.handle_message(s1, reddit_types.Join("frank")))
  let s3 = unwrap_state(reddit_engine.handle_message(s2, reddit_types.JoinSub("frank", "r/qux")))

  // Erin is not a member of r/qux, so creating a post should error
  let res = reddit_engine.handle_message(s3, reddit_types.CreatePost("erin", "r/qux", "Nope", "x"))
  case res {
    reddit_engine.EngineResult(_, reddit_types.Error(reason)) -> { assert reason == "not_member" }
    _ -> {
      assert unexpected("create_post_non_member_test")
      Nil
    }
  }
}

// Test: when the last member leaves a subreddit it should be removed
pub fn membership_last_member_removal_test() {
  let state0 = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)

  // alice joins and creates membership in r/solo
  let s1 = unwrap_state(reddit_engine.handle_message(state0, reddit_types.Join("alice")))
  let s2 = unwrap_state(reddit_engine.handle_message(s1, reddit_types.JoinSub("alice", "r/solo")))

  // confirm subreddit exists
  let exists_before = list.any(s2.subreddits, fn(e) { case e { reddit_engine.SubredditEntry(n, _) -> n == "r/solo" } })
  assert exists_before

  // alice leaves
  let res = reddit_engine.handle_message(s2, reddit_types.LeaveSub("alice", "r/solo"))
  case res {
    reddit_engine.EngineResult(s_after, reddit_types.Ok) -> {
      // subreddit should no longer exist
      let exists_after = list.any(s_after.subreddits, fn(e) { case e { reddit_engine.SubredditEntry(n, _) -> n == "r/solo" } })
      assert exists_after == False
      Nil
    }
    reddit_engine.EngineResult(_, reddit_types.Error(reason)) -> {
      assert unexpected("membership_last_member_removal_test leave error: " <> reason)
      Nil
    }
    _ -> {
      assert unexpected("membership_last_member_removal_test unexpected")
      Nil
    }
  }
}

// Test: posts from a subreddit are not accessible after the subreddit is removed
pub fn membership_last_member_posts_removed_test() {
  let state0 = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)

  let s1 = unwrap_state(reddit_engine.handle_message(state0, reddit_types.Join("alice")))
  let s2 = unwrap_state(reddit_engine.handle_message(s1, reddit_types.JoinSub("alice", "r/solo")))

  // create a post as alice
  let res_post = reddit_engine.handle_message(s2, reddit_types.CreatePost("alice", "r/solo", "T", "B"))
  case res_post {
    reddit_engine.EngineResult(s3, reddit_types.OkWithId(pid)) -> {
      // alice leaves, removing the subreddit
      let res_leave = reddit_engine.handle_message(s3, reddit_types.LeaveSub("alice", "r/solo"))
      case res_leave {
        reddit_engine.EngineResult(s_after, reddit_types.Ok) -> {
          // Attempt to get the post by id — should be not found
          let res_get = reddit_engine.handle_message(s_after, reddit_types.GetPost(pid))
          case res_get {
            reddit_engine.EngineResult(_, reddit_types.Error(reason)) -> {
              assert reason == "post_not_found"
              Nil
            }
            _ -> {
              assert unexpected("membership_last_member_posts_removed_test: post unexpectedly found")
              Nil
            }
          }
        }
        _ -> {
          assert unexpected("membership_last_member_posts_removed_test: leave failed")
          Nil
        }
      }
    }
    _ -> {
      assert unexpected("membership_last_member_posts_removed_test: create post failed")
      Nil
    }
  }
}
