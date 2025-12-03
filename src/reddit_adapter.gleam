// ============================================================================
// reddit_adapter.gleam - WITH BONUS CRYPTO SUPPORT
// ============================================================================
// Simple wrapper functions for interacting with the Reddit engine
// Updated to support RSA-2048 digital signatures
// ============================================================================

import reddit_engine
import reddit_types

// ============================================================================
// User Management (with Crypto)
// ============================================================================

/// Register a new user with RSA-2048 public key
/// If public_key is empty, server will generate keys automatically
pub fn register(
state: reddit_engine.EngineState,
user: String,
password: String,
public_key: String,
) {
  reddit_engine.handle_message(
  state,
  reddit_types.Register(user, password, public_key),
  )
}

/// Register a user with automatic key generation (empty public_key)
pub fn register_auto_keys(
state: reddit_engine.EngineState,
user: String,
password: String,
) {
  reddit_engine.handle_message(
  state,
  reddit_types.Register(user, password, ""),
  )
}

/// Login user
pub fn login(
state: reddit_engine.EngineState,
user: String,
password: String,
) {
  reddit_engine.handle_message(state, reddit_types.Login(user, password))
}

/// Get a user's public key
pub fn get_public_key(state: reddit_engine.EngineState, username: String) {
  reddit_engine.handle_message(state, reddit_types.GetPublicKey(username))
}

// ============================================================================
// Subreddit Management
// ============================================================================

/// Join a subreddit
pub fn join_sub(state: reddit_engine.EngineState, user: String, sub: String) {
  reddit_engine.handle_message(state, reddit_types.JoinSub(user, sub))
}

/// Leave a subreddit
pub fn leave_sub(state: reddit_engine.EngineState, user: String, sub: String) {
  reddit_engine.handle_message(state, reddit_types.LeaveSub(user, sub))
}

// ============================================================================
// Post Management (with Signatures)
// ============================================================================

/// Create a post with digital signature
pub fn create_post_signed(
state: reddit_engine.EngineState,
user: String,
sub: String,
title: String,
body: String,
signature: String,
) {
  reddit_engine.handle_message(
  state,
  reddit_types.CreatePost(user, sub, title, body, signature),
  )
}

/// Create an unsigned post (for backwards compatibility)
pub fn create_post(
state: reddit_engine.EngineState,
user: String,
sub: String,
title: String,
body: String,
) {
  reddit_engine.handle_message(
  state,
  reddit_types.CreatePost(user, sub, title, body, "unsigned"),
  )
}

/// Get a specific post by ID (signature will be verified by HTTP server)
pub fn get_post(state: reddit_engine.EngineState, post_id: Int) {
  reddit_engine.handle_message(state, reddit_types.GetPost(post_id))
}

/// Get user's personalized feed
pub fn get_feed(
state: reddit_engine.EngineState,
user: String,
page: Int,
page_size: Int,
) {
  reddit_engine.handle_message(
  state,
  reddit_types.GetFeed(user, page, page_size),
  )
}

/// Get subreddit feed
pub fn get_sub_feed(state: reddit_engine.EngineState, subreddit: String) {
  reddit_engine.handle_message(state, reddit_types.GetSubFeed(subreddit))
}

// ============================================================================
// Interactions
// ============================================================================

/// Vote on a post (delta: 1 for upvote, -1 for downvote, 0 to remove vote)
pub fn vote(
state: reddit_engine.EngineState,
user: String,
post_id: Int,
delta: Int,
) {
  reddit_engine.handle_message(state, reddit_types.Vote(user, post_id, delta))
}

/// Add a comment to a post
/// parent_id = 0 for top-level comments
pub fn create_comment(
state: reddit_engine.EngineState,
user: String,
post_id: Int,
parent_id: Int,
text: String,
) {
  reddit_engine.handle_message(
  state,
  reddit_types.CreateComment(user, post_id, parent_id, text),
  )
}

/// Add a top-level comment (convenience function)
pub fn comment_on_post(
state: reddit_engine.EngineState,
user: String,
post_id: Int,
text: String,
) {
  reddit_engine.handle_message(
  state,
  reddit_types.CreateComment(user, post_id, 0, text),
  )
}

// ============================================================================
// Direct Messaging
// ============================================================================

/// Send a direct message
pub fn send_dm(
state: reddit_engine.EngineState,
from: String,
to: String,
body: String,
) {
  reddit_engine.handle_message(
  state,
  reddit_types.SendDirectMessage(from, to, body),
  )
}

/// Get user's direct messages
pub fn get_dms(state: reddit_engine.EngineState, user: String) {
  reddit_engine.handle_message(state, reddit_types.GetDirectMessages(user))
}

// ============================================================================
// Utility Functions
// ============================================================================

/// Create initial empty engine state
pub fn initial_state() -> reddit_engine.EngineState {
  reddit_engine.EngineState([], [], [], [], 0, 0, 0)
}

/// Extract reply from engine result
pub fn get_reply(result: reddit_engine.EngineResult) -> reddit_types.EngineReply {
  case result {
    reddit_engine.EngineResult(_, reply) -> reply
  }
}

/// Extract state from engine result
pub fn get_state(result: reddit_engine.EngineResult) -> reddit_engine.EngineState {
  case result {
    reddit_engine.EngineResult(state, _) -> state
  }
}

/// Check if operation was successful
pub fn is_ok(reply: reddit_types.EngineReply) -> Bool {
  case reply {
    reddit_types.Ok -> True
    reddit_types.OkWithId(_) -> True
    _ -> False
  }
}

/// Check if operation failed
pub fn is_error(reply: reddit_types.EngineReply) -> Bool {
  case reply {
    reddit_types.Error(_) -> True
    _ -> False
  }
}

/// Extract error message if present
pub fn get_error(reply: reddit_types.EngineReply) -> String {
  case reply {
    reddit_types.Error(msg) -> msg
    _ -> "no_error"
  }
}

/// Extract post ID from OkWithId reply
pub fn get_id(reply: reddit_types.EngineReply) -> Int {
  case reply {
    reddit_types.OkWithId(id) -> id
    _ -> -1
  }
}

/// Extract posts from reply
pub fn get_posts(reply: reddit_types.EngineReply) -> List(reddit_types.Post) {
  case reply {
    reddit_types.Posts(posts) -> posts
    reddit_types.PostsPage(posts, _, _, _) -> posts
    _ -> []
  }
}

/// Extract post data from reply
pub fn get_post_data(reply: reddit_types.EngineReply) -> reddit_types.Post {
  case reply {
    reddit_types.PostData(post) -> post
    _ -> {
      // Return a dummy post if not found
      reddit_types.Post(
      -1,
      "unknown",
      "unknown",
      "Not Found",
      "Post not found",
      0,
      [],
      0,
      "unsigned",
      )
    }
  }
}

/// Extract public key from reply
pub fn get_public_key_data(reply: reddit_types.EngineReply) -> String {
  case reply {
    reddit_types.PublicKeyData(key) -> key
    _ -> ""
  }
}

/// Extract direct messages from reply
pub fn get_direct_messages(
reply: reddit_types.EngineReply,
) -> List(reddit_types.DirectMessage) {
  case reply {
    reddit_types.DirectMessages(messages) -> messages
    _ -> []
  }
}

// ============================================================================
// Example Usage Helper Functions
// ============================================================================

/// Complete user registration flow with automatic key generation
pub fn register_user_with_crypto(
state: reddit_engine.EngineState,
username: String,
password: String,
) {
  // Register with empty public_key to trigger auto-generation
  register_auto_keys(state, username, password)
}

/// Complete post creation flow with signature
pub fn create_verified_post(
state: reddit_engine.EngineState,
author: String,
subreddit: String,
title: String,
body: String,
signature: String,
) {
  create_post_signed(state, author, subreddit, title, body, signature)
}

/// Verify a user has a public key
pub fn has_public_key(
state: reddit_engine.EngineState,
username: String,
) -> Bool {
  let result = get_public_key(state, username)
  let reply = get_reply(result)
  case reply {
    reddit_types.PublicKeyData(key) -> key != ""
    _ -> False
  }
}