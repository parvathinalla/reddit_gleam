import reddit_engine
import reddit_types

// Each function returns #(state, reply) to be Erlang-friendly.
// You can add more wrappers if your Erlang demo needs them.

pub fn register(state: reddit_engine.EngineState, user: String, password: String) {
  reddit_engine.handle_message(state, reddit_types.Register(user, password))
}

pub fn join_sub(state: reddit_engine.EngineState, user: String, sub: String) {
  reddit_engine.handle_message(state, reddit_types.JoinSub(user, sub))
}

pub fn leave_sub(state: reddit_engine.EngineState, user: String, sub: String) {
  reddit_engine.handle_message(state, reddit_types.LeaveSub(user, sub))
}

pub fn create_post(state: reddit_engine.EngineState, user: String, sub: String, title: String, body: String) {
  reddit_engine.handle_message(state, reddit_types.CreatePost(user, sub, title, body))
}

pub fn create_comment(state: reddit_engine.EngineState, user: String, post_id: Int, text: String) {
  reddit_engine.handle_message(state, reddit_types.CreateComment(user, post_id, 0, text))
}

pub fn vote(state: reddit_engine.EngineState, user: String, post_id: Int, delta: Int) {
  reddit_engine.handle_message(state, reddit_types.Vote(user, post_id, delta))
}

pub fn get_feed(state: reddit_engine.EngineState, user: String, page: Int, page_size: Int) {
  reddit_engine.handle_message(state, reddit_types.GetFeed(user, page, page_size))
}

pub fn send_dm(state: reddit_engine.EngineState, from: String, to: String, body: String) {
  reddit_engine.handle_message(state, reddit_types.SendDirectMessage(from, to, body))
}

pub fn get_dms(state: reddit_engine.EngineState, user: String) {
  reddit_engine.handle_message(state, reddit_types.GetDirectMessages(user))
}