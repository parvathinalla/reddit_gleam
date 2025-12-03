import gleam/list
import reddit_engine
import reddit_types

// Helper used to create a non-constant failing assertion that the
// compiler won't flag as a redundant constant comparison.
fn unexpected(_s: String) -> Bool { False }

// Helper: unwrap EngineResult to the contained state (drop the reply)
fn unwrap_state(res: reddit_engine.EngineResult) -> reddit_engine.EngineState {
  case res { reddit_engine.EngineResult(s, _) -> s }
}

// Test that CreateComment inserts a top-level comment and preserves nested structure
pub fn comment_insertion_test() {
  let state0 = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)

  let s1 = unwrap_state(reddit_engine.handle_message(state0, reddit_types.Join("alice")))
  let s2 = unwrap_state(reddit_engine.handle_message(s1, reddit_types.Join("bob")))

  // Create a post and get the returned id
  case reddit_engine.handle_message(s2, reddit_types.CreatePost("alice", "r/test", "Hello", "World", signature: "")) {
    reddit_engine.EngineResult(s3, reddit_types.OkWithId(post_id)) -> {
      let s4 = unwrap_state(reddit_engine.handle_message(s3, reddit_types.CreateComment("bob", post_id, 0, "Nice post")))

      case reddit_engine.handle_message(s4, reddit_types.GetPost(post_id)) {
        reddit_engine.EngineResult(_s5, reddit_types.PostData(p)) -> case p { reddit_types.Post(_id, _author, _sub, _title, _body, _score, comments, _ts, _signature) -> {
            assert list.length(comments) == 1
            let found = list.any(comments, fn(c) { case c { reddit_types.Comment(_cid, author_c, body_c, _score_c, _replies, _ts_c) -> author_c == "bob" && body_c == "Nice post" } })
            assert found
            Nil
        } }
        _ -> {
          assert unexpected("comment_insertion_test: getpost unexpected")
          Nil
        }
      }
    }
    _ -> {
      assert unexpected("comment_insertion_test: create post unexpected")
      Nil
    }
  }
}

// Test register/login behaviour: duplicates and validation
pub fn registration_tests() {
  let state0 = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)

  case reddit_engine.handle_message(state0, reddit_types.Register("dave", "pwd", public_key: "")) {
    reddit_engine.EngineResult(s1, reddit_types.Ok) -> {
      // duplicate register should fail
      case reddit_engine.handle_message(s1, reddit_types.Register("dave", "pwd2", public_key: "")) {
        reddit_engine.EngineResult(_s2, reddit_types.Error(reason)) -> { assert reason == "user_exists" }
        _ -> {
          assert unexpected("registration_tests duplicate register unexpected")
          Nil
        }
      }

      // login succeeds with correct password
      case reddit_engine.handle_message(s1, reddit_types.Login("dave", "pwd")) {
  reddit_engine.EngineResult(_s3, reddit_types.Ok) -> { Nil }
        _ -> {
          assert unexpected("registration_tests login ok unexpected")
          Nil
        }
      }

      // login fails with wrong password
      case reddit_engine.handle_message(s1, reddit_types.Login("dave", "wrong")) {
  reddit_engine.EngineResult(_s4, reddit_types.Error(_)) -> { Nil }
        _ -> {
          assert unexpected("registration_tests login wrong unexpected")
          Nil
        }
      }
    }
  _ -> {
    assert unexpected("registration_tests top-level unexpected")
    Nil
  }
  }

  // invalid name (empty) rejected
  case reddit_engine.handle_message(state0, reddit_types.Register("", "x", public_key: "")) {
    reddit_engine.EngineResult(_s5, reddit_types.Error(reason2)) -> { assert reason2 == "invalid_name" }
      _ -> {
        assert unexpected("registration_tests invalid name unexpected")
        Nil
      }
  }
}

// Test vote idempotency and karma accounting per-voter
pub fn karma_votes_test() {
  let state0 = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)

  let s1 = unwrap_state(reddit_engine.handle_message(state0, reddit_types.Join("alice")))
  let s2 = unwrap_state(reddit_engine.handle_message(s1, reddit_types.Join("charlie")))

  case reddit_engine.handle_message(s2, reddit_types.CreatePost("alice", "r/test", "Post", "Body", signature: "")) {
    reddit_engine.EngineResult(s3, reddit_types.OkWithId(post_id)) -> {
      let s4 = unwrap_state(reddit_engine.handle_message(s3, reddit_types.Vote("charlie", post_id, 1)))

      case reddit_engine.handle_message(s4, reddit_types.GetPost(post_id)) {
        reddit_engine.EngineResult(_s5, reddit_types.PostData(p1)) -> case s4 {
          reddit_engine.EngineState(users1, _subs1, _votes1, _global_posts1, _pctr1, _cctr1, _ops1) -> case p1 { reddit_types.Post(_id1, _author1, _sub1, _title1, _body1, score1, _comments1, _ts1, _signature) -> {
              assert score1 == 1
              let alice_karma = list.fold(users1, 0, fn(acc, e) { case e { reddit_engine.UserEntry(n, u) -> case n == "alice" { True -> u.karma False -> acc } } })
              assert alice_karma == 1
              Nil
            } }
        }
        _ -> {
          assert unexpected("karma_votes_test outer unexpected 1")
          Nil
        }
      }

      // flip to downvote
      let s6 = unwrap_state(reddit_engine.handle_message(s4, reddit_types.Vote("charlie", post_id, -1)))
      case reddit_engine.handle_message(s6, reddit_types.GetPost(post_id)) {
  reddit_engine.EngineResult(_s7, reddit_types.PostData(p2)) -> case s6 { reddit_engine.EngineState(users2, _subs2, _votes2, _global_posts2, _pctr2, _cctr2, _ops2) -> case p2 { reddit_types.Post(_id2, _author2, _sub2, _title2, _body2, score2, _comments2, _ts2, _signature) -> {
              assert score2 == -1
              let alice_karma2 = list.fold(users2, 0, fn(acc, e) { case e { reddit_engine.UserEntry(n, u) -> case n == "alice" { True -> u.karma False -> acc } } })
              assert alice_karma2 == -1
              Nil
            } }
        }
        _ -> {
          assert unexpected("karma_votes_test outer unexpected 2")
          Nil
        }
      }
    }
  _ -> {
    assert unexpected("karma_votes_test top-level unexpected")
    Nil
  }
  }
}


