import gleam/list
import reddit_engine
import reddit_types

// Helper used to create a non-constant failing assertion
fn unexpected(_s: String) -> Bool { False }

// Helper: unwrap EngineResult to the contained state (drop the reply)
fn unwrap_state(res: reddit_engine.EngineResult) -> reddit_engine.EngineState {
  case res { reddit_engine.EngineResult(s, _) -> s }
}

// Collect post ids from a posts list
fn post_ids(posts: List(reddit_types.Post)) -> List(Int) {
  list.fold(posts, list.new(), fn(acc, p) {
    case p { reddit_types.Post(id, _, _, _, _, _, _, _) -> list.append(acc, [id]) }
  })
}

// Verify ordering (newest-first) and pagination semantics
pub fn feed_pagination_and_ordering_test() {
  let state0 = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)

  let s1 = unwrap_state(reddit_engine.handle_message(state0, reddit_types.Join("alice")))
  let s2 = unwrap_state(reddit_engine.handle_message(s1, reddit_types.Join("bob")))

  // alice posts first
  let s3 = unwrap_state(reddit_engine.handle_message(s2, reddit_types.CreatePost("alice", "r/a", "t1", "b1")))
  // bob posts second
  let s4 = unwrap_state(reddit_engine.handle_message(s3, reddit_types.CreatePost("bob", "r/b", "t2", "b2")))
  // alice posts third (newest)
  let s5 = unwrap_state(reddit_engine.handle_message(s4, reddit_types.CreatePost("alice", "r/a", "t3", "b3")))

  // Page 1 size 2 should return newest two posts (3,2)
  case reddit_engine.handle_message(s5, reddit_types.GetFeed("alice", 1, 2)) {
    reddit_engine.EngineResult(_s6, reddit_types.PostsPage(posts, page, page_size, total)) -> {
      let ids = post_ids(posts)
      case ids {
        [first, second, .._] -> {
          assert first == 3
          assert second == 2
          assert page == 1
          assert page_size == 2
          assert total == 3
          Nil
        }
        _ -> {
          assert unexpected("feed_pagination_and_ordering_test: unexpected posts shape")
          Nil
        }
      }
    }
  _ -> { assert unexpected("feed_pagination_and_ordering_test: unexpected reply") Nil }
  }
}

// Verify subscription filtering: a user only sees posts from their subscribed subreddits
pub fn feed_subscription_filtering_test() {
  let state0 = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)

  let s1 = unwrap_state(reddit_engine.handle_message(state0, reddit_types.Join("alice")))
  let s2 = unwrap_state(reddit_engine.handle_message(s1, reddit_types.Join("bob")))

  // bob posts in two different subreddits
  let s3 = unwrap_state(reddit_engine.handle_message(s2, reddit_types.CreatePost("bob", "r/a", "tA", "bA")))
  let s4 = unwrap_state(reddit_engine.handle_message(s3, reddit_types.CreatePost("bob", "r/b", "tB", "bB")))

  // alice subscribes only to r/a
  let s5 = unwrap_state(reddit_engine.handle_message(s4, reddit_types.JoinSub("alice", "r/a")))

  case reddit_engine.handle_message(s5, reddit_types.GetFeed("alice", 1, 10)) {
    reddit_engine.EngineResult(_s6, reddit_types.PostsPage(posts, page, page_size, total)) -> {
      let ids = post_ids(posts)
      // only one post should be visible (r/a)
      assert total == 1
  case ids { [id_a, .._] -> { assert id_a == 1 Nil } _ -> { assert unexpected("feed_subscription_filtering_test: ids shape") Nil } }
      assert page == 1
      assert page_size == 10
      Nil
    }
  _ -> { assert unexpected("feed_subscription_filtering_test: unexpected reply") Nil }
  }
}

// Verify defaults: page <=0 and page_size <=0 get normalized to page=1, page_size=10
pub fn feed_defaults_test() {
  let state0 = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)

  let s1 = unwrap_state(reddit_engine.handle_message(state0, reddit_types.Join("u")))
  // create a single post
  let s2 = unwrap_state(reddit_engine.handle_message(s1, reddit_types.CreatePost("u", "r/x", "t", "b")))

  case reddit_engine.handle_message(s2, reddit_types.GetFeed("u", 0, 0)) {
    reddit_engine.EngineResult(_s3, reddit_types.PostsPage(_posts, page, page_size, total)) -> {
      assert page == 1
      assert page_size == 10
      assert total == 1
      Nil
    }
    _ -> { assert unexpected("feed_defaults_test: unexpected reply") Nil }
  }
}

// Ensure joining the same subreddit twice doesn't duplicate entry in user's subreddits
pub fn feed_joinsub_idempotency_test() {
  let state0 = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)

  let s1 = unwrap_state(reddit_engine.handle_message(state0, reddit_types.Join("alice")))
  let s2 = unwrap_state(reddit_engine.handle_message(s1, reddit_types.JoinSub("alice", "r/x")))
  let s3 = unwrap_state(reddit_engine.handle_message(s2, reddit_types.JoinSub("alice", "r/x")))

  case s3 { reddit_engine.EngineState(users, _subs, _votes, _global_posts, _p, _c, _ops) -> {
    let cnt = list.fold(users, 0, fn(acc, e) {
      case e { reddit_engine.UserEntry(n, u) -> case n == "alice" {
        True -> list.fold(u.subreddits, acc, fn(acc2, r) { case r { rr -> case rr == "r/x" { True -> acc2 + 1 False -> acc2 } } })
        False -> acc
      } }
    })
    assert cnt == 1
    Nil
  } }
}
