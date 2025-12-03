import gleam/list
import reddit_engine
import reddit_types

fn unwrap_state(res: reddit_engine.EngineResult) -> reddit_engine.EngineState {
  case res { reddit_engine.EngineResult(s, _) -> s }
}

pub fn list_subreddits_counts_test() {
  let state0 = reddit_engine.EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)

  // setup: two subs
  let s1 = unwrap_state(reddit_engine.handle_message(state0, reddit_types.Join("alice")))
  let s2 = unwrap_state(reddit_engine.handle_message(s1, reddit_types.JoinSub("alice", "r/a")))
  let s3 = unwrap_state(reddit_engine.handle_message(s2, reddit_types.Join("bob")))
  let s4 = unwrap_state(reddit_engine.handle_message(s3, reddit_types.JoinSub("bob", "r/a")))

  // create a post in r/a
  let s5 = unwrap_state(reddit_engine.handle_message(s4, reddit_types.CreatePost("alice", "r/a", "T", "B", "")))

  // create r/b with one member
  let s6 = unwrap_state(reddit_engine.handle_message(s5, reddit_types.Join("carol")))
  let s7 = unwrap_state(reddit_engine.handle_message(s6, reddit_types.JoinSub("carol", "r/b")))

  let subs = reddit_engine.list_subreddits(s7)
  // subs is a list of tuples (name, members, posts); find entries
  let a = list.any(subs, fn(t) { case t { reddit_types.Subreddit(n, m, p) -> n == "r/a" && list.length(m) == 2 && list.length(p) == 1 } })
  let b = list.any(subs, fn(t) { case t { reddit_types.Subreddit(n, m, p) -> n == "r/b" && list.length(m) == 1 && p == [] } })

  assert a
  assert b
}
