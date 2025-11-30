pub type User {
  User(
  name: String,
  karma: Int,
  inbox: List(DirectMessage),
  subreddits: List(String),
  password: String,
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

// Messages that can be sent to the engine.
pub type EngineMsg {
  Join(name: String) // register account
  JoinSub(user: String, subreddit: String)
  LeaveSub(user: String, subreddit: String)
  CreatePost(author: String, subreddit: String, title: String, body: String)
  Vote(voter: String, post_id: Int, delta: Int)
  Register(name: String, password: String)
  Login(name: String, password: String)
  // Read APIs / richer ops
  GetFeed(user: String, page: Int, page_size: Int) // return a paginated list of posts relevant to user
  GetSubFeed(subreddit: String) // return posts for a subreddit
  GetPost(post_id: Int) // fetch a single post + comments
  // Commenting: parent_comment_id = 0 indicates root comment
  CreateComment(
  author: String,
  post_id: Int,
  parent_comment_id: Int,
  body: String,
  )
  SendDirectMessage(from: String, to: String, body: String)
  GetDirectMessages(user: String)
}

// Replies the engine can return for a request.
pub type EngineReply {
  Ok
  OkWithId(Int)
  Error(String)
  Posts(List(Post))
  PostsPage(List(Post), Int, Int, Int) // posts, page, page_size, total
  PostData(Post)
  UserData(User)
  DirectMessages(List(DirectMessage))
}
//
//pub type User {
//  User(
//  name: String,
//  karma: Int,
//  inbox: List(DirectMessage),
//  subreddits: List(String),
//  password: String,
//  public_key: String,      // NEW for BONUS
//  private_key: String,     // NEW for BONUS
//  )
//}
//
//pub type Subreddit {
//  Subreddit(name: String, members: List(String), posts: List(Post))
//}
//
//pub type Post {
//  Post(
//  id: Int,
//  author: String,
//  subreddit: String,
//  title: String,
//  body: String,
//  score: Int,
//  comments: List(Comment),
//  timestamp: Int,
//  signature: String,       // NEW for BONUS
//  )
//}
//
//pub type Comment {
//  Comment(
//  id: Int,
//  author: String,
//  body: String,
//  score: Int,
//  replies: List(Comment),
//  timestamp: Int,
//  )
//}
//
//pub type DirectMessage {
//  DirectMessage(from: String, to: String, body: String, timestamp: Int)
//}
//
//pub type ClientConfig {
//  ClientConfig(ai_model: String, ai_model_enabled: Bool)
//}
//
//// Messages that can be sent to the engine
//pub type EngineMsg {
//  Join(name: String)
//  JoinSub(user: String, subreddit: String)
//  LeaveSub(user: String, subreddit: String)
//  CreatePost(author: String, subreddit: String, title: String, body: String, signature: String)
//  Vote(voter: String, post_id: Int, delta: Int)
//  Register(name: String, password: String, public_key: String)
//  Login(name: String, password: String)
//  GetFeed(user: String, page: Int, page_size: Int)
//  GetSubFeed(subreddit: String)
//  GetPost(post_id: Int)
//  CreateComment(author: String, post_id: Int, parent_comment_id: Int, body: String)
//  SendDirectMessage(from: String, to: String, body: String)
//  GetDirectMessages(user: String)
//  GetPublicKey(username: String)  // NEW for BONUS
//  ListSubreddits  // NEW for REST API
//}
//
//// Replies the engine can return
//pub type EngineReply {
//  Ok
//  OkWithId(Int)
//  Error(String)
//  Posts(List(Post))
//  PostsPage(List(Post), Int, Int, Int)
//  PostData(Post)
//  UserData(User)
//  DirectMessages(List(DirectMessage))
//  PublicKey(String)  // NEW for BONUS
//  Subreddits(List(Subreddit))  // NEW for REST API
//}