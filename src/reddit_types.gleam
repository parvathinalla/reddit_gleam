pub type User {
  User(
  name: String,
  karma: Int,
  inbox: List(DirectMessage),
  subreddits: List(String),
  password: String,
  public_key: String,  // ← ADDED for bonus
  )
}

pub type Subreddit {
  Subreddit(name: String, members: List(String), posts: List(Post))
}

pub type Post {
  Post(
  id: Int,
  author: String,
  subreddit: String,
  title: String,
  body: String,
  score: Int,
  comments: List(Comment),
  timestamp: Int,
  signature: String,  // ← ADDED for bonus
  )
}

pub type Comment {
  Comment(
  id: Int,
  author: String,
  body: String,
  score: Int,
  replies: List(Comment),
  timestamp: Int,
  )
}

pub type DirectMessage {
  DirectMessage(from: String, to: String, body: String, timestamp: Int)
}

pub type ClientConfig {
  ClientConfig(ai_model: String, ai_model_enabled: Bool)
}

// Messages that can be sent to the engine
pub type EngineMsg {
  Join(name: String)
  JoinSub(user: String, subreddit: String)
  LeaveSub(user: String, subreddit: String)
  CreatePost(
  author: String,
  subreddit: String,
  title: String,
  body: String,
  signature: String,  // ← ADDED for bonus
  )
  Vote(voter: String, post_id: Int, delta: Int)
  Register(name: String, password: String, public_key: String)  // ← UPDATED for bonus
  Login(name: String, password: String)
  GetFeed(user: String, page: Int, page_size: Int)
  GetSubFeed(subreddit: String)
  GetPost(post_id: Int)
  CreateComment(
  author: String,
  post_id: Int,
  parent_comment_id: Int,
  body: String,
  )
  SendDirectMessage(from: String, to: String, body: String)
  GetDirectMessages(user: String)
  GetPublicKey(username: String)  // ← ADDED for bonus
}

// Replies the engine can return for a request
pub type EngineReply {
  Ok
  OkWithId(Int)
  Error(String)
  Posts(List(Post))
  PostsPage(List(Post), Int, Int, Int)  // posts, page, page_size, total
  PostData(Post)
  UserData(User)
  DirectMessages(List(DirectMessage))
  PublicKeyData(String)  // ← ADDED for bonus
}