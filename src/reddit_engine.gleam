import gleam/io
import gleam/list
import gleam/int
import reddit_types
import reddit_metrics

pub type VoteEntry {
  VoteEntry(
  post_id: Int,
  voter: String,
  value: Int
  )
}

pub type EngineState {
  EngineState(
  users: List(UserEntry),
  subreddits: List(SubredditEntry),
  votes: List(VoteEntry),
  global_posts: List(reddit_types.Post),
  post_id_counter: Int,
  comment_id_counter: Int,
  operations: Int
  )
}

pub type EngineResult {
  EngineResult(
  EngineState,
  reddit_types.EngineReply
  )
}

pub type MaybePost {
  Found(reddit_types.Post)
  NotFound
}

pub type MaybeUser {
  UserFound(reddit_types.User)
  UserNotFound
}

pub type InsertResult {
  InsertResult(List(reddit_types.Comment), Bool)
}

pub type PostInsertAcc {
  PostInsertAcc(List(reddit_types.Post), Bool)
}

pub type SubredditInsertAcc {
  SubredditInsertAcc(List(SubredditEntry), Bool)
}

pub type PagAcc {
  PagAcc(List(reddit_types.Post), Int)
}

pub type UserEntry {
  UserEntry(
  name: String,
  user: reddit_types.User
  )
}

pub type SubredditEntry {
  SubredditEntry(
  name: String,
  subreddit: reddit_types.Subreddit
  )
}

pub fn start() {
  engine_loop(
  EngineState([], [], [], [], 0, 0, 0)
  )
}

// ============= HELPER FUNCTIONS (Pattern Matching) =============

fn check_if_user_exists(users: List(UserEntry), target_name: String) -> Bool {
  case users {
    [] -> False
    [first, ..rest] -> {
      case first {
        UserEntry(username, _) -> {
          case username == target_name {
            True -> True
            False -> check_if_user_exists(rest, target_name)
          }
        }
      }
    }
  }
}

fn find_user(users: List(UserEntry), name: String) -> MaybeUser {
  case users {
    [] -> UserNotFound
    [first, ..rest] -> {
      case first {
        UserEntry(username, user) -> {
          case username == name {
            True -> UserFound(user)
            False -> find_user(rest, name)
          }
        }
      }
    }
  }
}

fn subreddit_exists(subs: List(SubredditEntry), target: String) -> Bool {
  case subs {
    [] -> False
    [first, ..rest] -> {
      case first {
        SubredditEntry(name, _) -> {
          case name == target {
            True -> True
            False -> subreddit_exists(rest, target)
          }
        }
      }
    }
  }
}

fn is_user_in_members(members: List(String), target: String) -> Bool {
  case members {
    [] -> False
    [first, ..rest] -> {
      case first == target {
        True -> True
        False -> is_user_in_members(rest, target)
      }
    }
  }
}

fn is_member_of_subreddit(subs: List(SubredditEntry), username: String, subreddit_name: String) -> Bool {
  case subs {
    [] -> False
    [first, ..rest] -> {
      case first {
        SubredditEntry(name, sub) -> {
          case name == subreddit_name {
            True -> is_user_in_members(sub.members, username)
            False -> is_member_of_subreddit(rest, username, subreddit_name)
          }
        }
      }
    }
  }
}

fn is_string_in_list(strings: List(String), target: String) -> Bool {
  case strings {
    [] -> False
    [first, ..rest] -> {
      case first == target {
        True -> True
        False -> is_string_in_list(rest, target)
      }
    }
  }
}

// ============= MESSAGE HANDLER =============

pub fn handle_message(state: EngineState, msg: reddit_types.EngineMsg) -> EngineResult {
  let new_ops = state.operations + 1
  case msg {
    reddit_types.Join(name) -> {
      let _ = reddit_metrics.log_event("Join: " <> name)
      let new_state = add_user(state, name, new_ops)
      EngineResult(new_state, reddit_types.Ok)
    }

    reddit_types.JoinSub(user, subreddit) -> {
      let _ = reddit_metrics.log_event("JoinSub: " <> user <> " -> " <> subreddit)
      let state1 = add_user(state, user, new_ops)

      let exists = subreddit_exists(state1.subreddits, subreddit)
      case exists {
        False -> {
          let new_sub = SubredditEntry(subreddit, reddit_types.Subreddit(subreddit, [user], []))
          let final = list.append(state1.subreddits, [new_sub])

          let updated_users = update_user_subreddits(state1.users, user, subreddit)
          EngineResult(EngineState(updated_users, final, state1.votes, state1.global_posts, state1.post_id_counter, state1.comment_id_counter, new_ops), reddit_types.Ok)
        }
        True -> {
          let already = is_member_of_subreddit(state1.subreddits, user, subreddit)
          case already {
            True -> EngineResult(EngineState(state1.users, state1.subreddits, state1.votes, state1.global_posts, state1.post_id_counter, state1.comment_id_counter, new_ops), reddit_types.Error("already_member"))
            False -> {
              let updated = add_user_to_subreddit(state1.subreddits, subreddit, user)
              let updated_users = update_user_subreddits(state1.users, user, subreddit)
              EngineResult(EngineState(updated_users, updated, state1.votes, state1.global_posts, state1.post_id_counter, state1.comment_id_counter, new_ops), reddit_types.Ok)
            }
          }
        }
      }
    }

    reddit_types.LeaveSub(user, subreddit) -> {
      let _ = reddit_metrics.log_event("LeaveSub: " <> user <> " -> " <> subreddit)
      let found = subreddit_exists(state.subreddits, subreddit)
      case found {
        False -> EngineResult(EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops), reddit_types.Error("subreddit_not_found"))
        True -> {
          let member = is_member_of_subreddit(state.subreddits, user, subreddit)
          case member {
            False -> EngineResult(EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops), reddit_types.Error("not_member"))
            True -> {
              let new_state = leave_sub(state, user, subreddit, new_ops)
              EngineResult(new_state, reddit_types.Ok)
            }
          }
        }
      }
    }

    reddit_types.CreatePost(author, subreddit, title, body) -> {
      let _ = reddit_metrics.log_event("CreatePost by " <> author <> " in " <> subreddit)
      let sub_exists = subreddit_exists(state.subreddits, subreddit)
      case sub_exists {
        True -> {
          let is_member = is_member_of_subreddit(state.subreddits, author, subreddit)
          case is_member {
            True -> {
              let new_state = create_post(state, author, subreddit, title, body, new_ops)
              case new_state { EngineState(_, _, _, _, post_id_counter, _, _) -> EngineResult(new_state, reddit_types.OkWithId(post_id_counter)) }
            }
            False -> {
              EngineResult(EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops), reddit_types.Error("not_member"))
            }
          }
        }
        False -> {
          let new_sub = SubredditEntry(subreddit, reddit_types.Subreddit(subreddit, [author], []))
          let state1 = EngineState(state.users, list.append(state.subreddits, [new_sub]), state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops)
          let new_state = create_post(state1, author, subreddit, title, body, new_ops)
          case new_state { EngineState(_, _, _, _, post_id_counter, _, _) -> EngineResult(new_state, reddit_types.OkWithId(post_id_counter)) }
        }
      }
    }

    reddit_types.Vote(voter, post_id, delta) -> {
      let _ = reddit_metrics.log_event("Vote: post " <> int.to_string(post_id) <> " voter=" <> voter <> " delta=" <> int.to_string(delta))
      let new_state = vote_post(state, voter, post_id, delta, new_ops)
      EngineResult(new_state, reddit_types.Ok)
    }

    reddit_types.Register(name, password) -> {
      let _ = reddit_metrics.log_event("Register: " <> name)
      let user_exists_result = check_if_user_exists(state.users, name)
      case user_exists_result {
        True -> {
          EngineResult(
          EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops),
          reddit_types.Error("user_exists")
          )
        }
        False -> {
          case name {
            "" -> {
              EngineResult(
              EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops),
              reddit_types.Error("invalid_name")
              )
            }
            _ -> {
              let user = reddit_types.User(name, 0, [], [], password)
              let entry = UserEntry(name, user)
              let users = list.append(state.users, [entry])
              EngineResult(
              EngineState(users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops),
              reddit_types.Ok
              )
            }
          }
        }
      }
    }

    reddit_types.Login(name, password) -> {
      let _ = reddit_metrics.log_event("Login: " <> name <> " with password")
      let maybe = find_user(state.users, name)
      case maybe {
        UserNotFound -> {
          let _ = reddit_metrics.log_event("Login: User not found: " <> name)
          EngineResult(
          EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops),
          reddit_types.Error("login_failed")
          )
        }
        UserFound(u) -> {
          case u {
            reddit_types.User(_n, _karma, _inbox, _subs, pw) -> {
              let _ = reddit_metrics.log_event("Login: Comparing passwords")
              case pw == password {
                True -> {
                  let _ = reddit_metrics.log_event("Login: Password match!")
                  EngineResult(
                  EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops),
                  reddit_types.Ok
                  )
                }
                False -> {
                  let _ = reddit_metrics.log_event("Login: Password mismatch!")
                  EngineResult(
                  EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops),
                  reddit_types.Error("login_failed")
                  )
                }
              }
            }
          }
        }
      }
    }

    reddit_types.GetSubFeed(subreddit_name) -> {
      let _ = reddit_metrics.log_event("GetSubFeed: " <> subreddit_name)
      let posts = filter_posts_by_subreddit(state.global_posts, subreddit_name)
      let sorted = sort_posts_by_ts_desc(posts)
      let new_state = EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops)
      EngineResult(new_state, reddit_types.Posts(sorted))
    }

    reddit_types.GetFeed(user, page, page_size) -> {
      let _ = reddit_metrics.log_event("GetFeed: " <> user)
      let maybe_user = find_user(state.users, user)
      case maybe_user {
        UserNotFound -> {
          let new_state = EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops)
          EngineResult(new_state, reddit_types.Error("user_not_found"))
        }
        UserFound(u) -> {
          let allowed = u.subreddits
          let posts_all = state.global_posts
          let posts = case allowed {
            [] -> posts_all
            _ -> filter_posts_by_subreddits(state.global_posts, allowed)
          }
          let total = list.length(posts)
          let page_safe = case page <= 0 { True -> 1 False -> page }
          let page_size_safe = case page_size <= 0 { True -> 10 False -> page_size }
          let skip = { page_safe - 1 } * page_size_safe
          let sorted = sort_posts_by_ts_desc(posts)
          let page_posts = paginate(sorted, skip, page_size_safe)
          let new_state = EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops)
          EngineResult(new_state, reddit_types.PostsPage(page_posts, page_safe, page_size_safe, total))
        }
      }
    }

    reddit_types.GetPost(post_id) -> {
      let _ = reddit_metrics.log_event("GetPost: " <> int.to_string(post_id))
      let maybe_post = find_post(state.subreddits, post_id)
      let new_state = EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops)
      case maybe_post {
        Found(p) -> EngineResult(new_state, reddit_types.PostData(p))
        NotFound -> EngineResult(new_state, reddit_types.Error("post_not_found"))
      }
    }

    reddit_types.CreateComment(author, post_id, parent_id, body) -> {
      let _ = reddit_metrics.log_event("CreateComment by " <> author <> " on post " <> int.to_string(post_id))
      let new_comment_id = state.comment_id_counter + 1
      let new_comment = reddit_types.Comment(new_comment_id, author, body, 0, [], new_ops)

      let result = insert_comment_into_subreddits(state.subreddits, post_id, parent_id, new_comment)
      case result {
        SubredditInsertAcc(new_subs, True) -> {
          let maybe_updated = find_post(new_subs, post_id)
          let updated_global = case maybe_updated {
            Found(p) -> update_post_in_global(state.global_posts, p)
            NotFound -> state.global_posts
          }
          let new_state = EngineState(state.users, new_subs, state.votes, updated_global, state.post_id_counter, new_comment_id, new_ops)
          EngineResult(new_state, reddit_types.OkWithId(new_comment_id))
        }
        SubredditInsertAcc(_, False) -> {
          let new_state = EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops)
          EngineResult(new_state, reddit_types.Error("post_or_parent_not_found"))
        }
      }
    }

    reddit_types.SendDirectMessage(from, to, body) -> {
      let _ = reddit_metrics.log_event("SendDirectMessage from " <> from <> " to " <> to)
      let new_users = send_dm_to_user(state.users, from, to, body, new_ops)
      let new_state = EngineState(new_users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops)
      EngineResult(new_state, reddit_types.Ok)
    }

    reddit_types.GetDirectMessages(user) -> {
      let _ = reddit_metrics.log_event("GetDirectMessages for " <> user)
      let maybe_user = find_user(state.users, user)
      case maybe_user {
        UserNotFound -> EngineResult(EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops), reddit_types.Error("user_not_found"))
        UserFound(u) -> {
          let new_state = EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops)
          EngineResult(new_state, reddit_types.DirectMessages(u.inbox))
        }
      }
    }
  }
}

// ============= HELPER FUNCTIONS FOR UPDATES =============

fn update_user_subreddits(users: List(UserEntry), username: String, subreddit: String) -> List(UserEntry) {
  case users {
    [] -> []
    [first, ..rest] -> {
      case first {
        UserEntry(name, u) -> {
          case name == username {
            True -> {
              case is_string_in_list(u.subreddits, subreddit) {
                True -> list.append([first], update_user_subreddits(rest, username, subreddit))
                False -> {
                  let updated_user = reddit_types.User(u.name, u.karma, u.inbox, list.append(u.subreddits, [subreddit]), u.password)
                  list.append([UserEntry(name, updated_user)], update_user_subreddits(rest, username, subreddit))
                }
              }
            }
            False -> list.append([first], update_user_subreddits(rest, username, subreddit))
          }
        }
      }
    }
  }
}

fn add_user_to_subreddit(subs: List(SubredditEntry), subreddit_name: String, user: String) -> List(SubredditEntry) {
  case subs {
    [] -> []
    [first, ..rest] -> {
      case first {
        SubredditEntry(name, sub) -> {
          case name == subreddit_name {
            True -> {
              let updated_sub = reddit_types.Subreddit(name, list.append(sub.members, [user]), sub.posts)
              list.append([SubredditEntry(name, updated_sub)], add_user_to_subreddit(rest, subreddit_name, user))
            }
            False -> list.append([first], add_user_to_subreddit(rest, subreddit_name, user))
          }
        }
      }
    }
  }
}

fn filter_posts_by_subreddit(posts: List(reddit_types.Post), subreddit: String) -> List(reddit_types.Post) {
  case posts {
    [] -> []
    [first, ..rest] -> {
      case first {
        reddit_types.Post(_id, _author, sub_name, _t, _b, _s, _c, _ts) -> {
          case sub_name == subreddit {
            True -> list.append([first], filter_posts_by_subreddit(rest, subreddit))
            False -> filter_posts_by_subreddit(rest, subreddit)
          }
        }
      }
    }
  }
}

fn filter_posts_by_subreddits(posts: List(reddit_types.Post), allowed: List(String)) -> List(reddit_types.Post) {
  case posts {
    [] -> []
    [first, ..rest] -> {
      case first {
        reddit_types.Post(_id, _author, sub_name, _t, _b, _s, _c, _ts) -> {
          case is_string_in_list(allowed, sub_name) {
            True -> list.append([first], filter_posts_by_subreddits(rest, allowed))
            False -> filter_posts_by_subreddits(rest, allowed)
          }
        }
      }
    }
  }
}

fn update_post_in_global(posts: List(reddit_types.Post), updated_post: reddit_types.Post) -> List(reddit_types.Post) {
  case posts {
    [] -> []
    [first, ..rest] -> {
      case first {
        reddit_types.Post(id_g, _, _, _, _, _, _, _) -> {
          case updated_post {
            reddit_types.Post(id_p, _, _, _, _, _, _, _) -> {
              case id_g == id_p {
                True -> list.append([updated_post], update_post_in_global(rest, updated_post))
                False -> list.append([first], update_post_in_global(rest, updated_post))
              }
            }
          }
        }
      }
    }
  }
}

fn send_dm_to_user(users: List(UserEntry), from: String, to: String, body: String, timestamp: Int) -> List(UserEntry) {
  case users {
    [] -> []
    [first, ..rest] -> {
      case first {
        UserEntry(name, u) -> {
          case name == to {
            True -> {
              let msg = reddit_types.DirectMessage(from, to, body, timestamp)
              let new_inbox = list.append(u.inbox, [msg])
              let updated_user = reddit_types.User(u.name, u.karma, new_inbox, u.subreddits, u.password)
              list.append([UserEntry(name, updated_user)], send_dm_to_user(rest, from, to, body, timestamp))
            }
            False -> list.append([first], send_dm_to_user(rest, from, to, body, timestamp))
          }
        }
      }
    }
  }
}

fn insert_comment_into_subreddits(subs: List(SubredditEntry), post_id: Int, parent_id: Int, new_comment: reddit_types.Comment) -> SubredditInsertAcc {
  case subs {
    [] -> SubredditInsertAcc([], False)
    [first, ..rest] -> {
      case first {
        SubredditEntry(name, subreddit) -> {
          let result = insert_comment_into_posts(subreddit.posts, post_id, parent_id, new_comment)
          case result {
            PostInsertAcc(new_posts, True) -> {
              let new_sub = SubredditEntry(name, reddit_types.Subreddit(name, subreddit.members, new_posts))
              SubredditInsertAcc(list.append([new_sub], rest), True)
            }
            PostInsertAcc(new_posts, False) -> {
              let prev_result = insert_comment_into_subreddits(rest, post_id, parent_id, new_comment)
              case prev_result {
                SubredditInsertAcc(remaining_subs, found) -> {
                  let new_sub = SubredditEntry(name, reddit_types.Subreddit(name, subreddit.members, new_posts))
                  SubredditInsertAcc(list.append([new_sub], remaining_subs), found)
                }
              }
            }
          }
        }
      }
    }
  }
}

fn insert_comment_into_posts(posts: List(reddit_types.Post), post_id: Int, parent_id: Int, new_comment: reddit_types.Comment) -> PostInsertAcc {
  case posts {
    [] -> PostInsertAcc([], False)
    [first, ..rest] -> {
      case first {
        reddit_types.Post(id, author_p, subreddit_name_p, title_p, body_p, score_p, comments_p, ts_p) -> {
          case id == post_id {
            True -> {
              case parent_id == 0 {
                True -> {
                  let new_comments = list.append(comments_p, [new_comment])
                  let new_post = reddit_types.Post(id, author_p, subreddit_name_p, title_p, body_p, score_p, new_comments, ts_p)
                  PostInsertAcc(list.append([new_post], rest), True)
                }
                False -> {
                  let nested = insert_into_comments(comments_p, parent_id, new_comment)
                  case nested {
                    InsertResult(new_comments, True) -> {
                      let new_post = reddit_types.Post(id, author_p, subreddit_name_p, title_p, body_p, score_p, new_comments, ts_p)
                      PostInsertAcc(list.append([new_post], rest), True)
                    }
                    InsertResult(_, False) -> {
                      PostInsertAcc(list.append([first], rest), False)
                    }
                  }
                }
              }
            }
            False -> {
              let prev_result = insert_comment_into_posts(rest, post_id, parent_id, new_comment)
              case prev_result {
                PostInsertAcc(remaining_posts, found) -> {
                  PostInsertAcc(list.append([first], remaining_posts), found)
                }
              }
            }
          }
        }
      }
    }
  }
}

fn insert_into_comments(comments: List(reddit_types.Comment), parent_id: Int, new_comment: reddit_types.Comment) -> InsertResult {
  case comments {
    [] -> InsertResult([], False)
    [first, ..rest] -> {
      case first {
        reddit_types.Comment(id, author_c, body_c, score_c, replies_c, ts_c) -> {
          case id == parent_id {
            True -> {
              let new_replies = list.append(replies_c, [new_comment])
              InsertResult(list.append([reddit_types.Comment(id, author_c, body_c, score_c, new_replies, ts_c)], rest), True)
            }
            False -> {
              let nested = insert_into_comments(replies_c, parent_id, new_comment)
              case nested {
                InsertResult(new_replies, True) -> {
                  InsertResult(list.append([reddit_types.Comment(id, author_c, body_c, score_c, new_replies, ts_c)], rest), True)
                }
                InsertResult(_, False) -> {
                  let prev_result = insert_into_comments(rest, parent_id, new_comment)
                  case prev_result {
                    InsertResult(remaining_comments, found) -> {
                      InsertResult(list.append([first], remaining_comments), found)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

// ============= SORTING AND PAGINATION =============

fn insert_sorted(sorted_acc: List(reddit_types.Post), p: reddit_types.Post) -> List(reddit_types.Post) {
  case sorted_acc {
    [] -> [p]
    [h, ..t] -> {
      case h {
        reddit_types.Post(_, _, _, _, _, _, _, ts_h) -> {
          case p {
            reddit_types.Post(_, _, _, _, _, _, _, ts_p) -> {
              case ts_p >= ts_h {
                True -> list.append([p], sorted_acc)
                False -> list.append([h], insert_sorted(t, p))
              }
            }
          }
        }
      }
    }
  }
}

fn sort_posts_by_ts_desc(posts: List(reddit_types.Post)) -> List(reddit_types.Post) {
  case posts {
    [] -> []
    [first, ..rest] -> {
      let sorted_rest = sort_posts_by_ts_desc(rest)
      insert_sorted(sorted_rest, first)
    }
  }
}

fn find_post(subs: List(SubredditEntry), post_id: Int) -> MaybePost {
  case subs {
    [] -> NotFound
    [first, ..rest] -> {
      case first {
        SubredditEntry(_, subreddit) -> {
          let result = find_post_in_posts(subreddit.posts, post_id)
          case result {
            Found(p) -> Found(p)
            NotFound -> find_post(rest, post_id)
          }
        }
      }
    }
  }
}

fn find_post_in_posts(posts: List(reddit_types.Post), post_id: Int) -> MaybePost {
  case posts {
    [] -> NotFound
    [first, ..rest] -> {
      case first {
        reddit_types.Post(id, _, _, _, _, _, _, _) -> {
          case id == post_id {
            True -> Found(first)
            False -> find_post_in_posts(rest, post_id)
          }
        }
      }
    }
  }
}

fn paginate(xs: List(reddit_types.Post), skip: Int, count: Int) -> List(reddit_types.Post) {
  let end = skip + count
  let acc = paginate_helper(xs, 0, skip, end, [])
  acc
}

fn paginate_helper(xs: List(reddit_types.Post), idx: Int, skip: Int, end: Int, acc: List(reddit_types.Post)) -> List(reddit_types.Post) {
  case xs {
    [] -> acc
    [first, ..rest] -> {
      case idx >= skip && idx < end {
        True -> paginate_helper(rest, idx + 1, skip, end, list.append(acc, [first]))
        False -> paginate_helper(rest, idx + 1, skip, end, acc)
      }
    }
  }
}

// ============= STATE MODIFICATION FUNCTIONS =============

fn add_user(state: EngineState, name: String, new_ops: Int) -> EngineState {
  let exists = check_if_user_exists(state.users, name)
  case exists {
    True -> EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops)
    False -> {
      let user = reddit_types.User(name, 0, [], [], "")
      let entry = UserEntry(name, user)
      let users = list.append(state.users, [entry])
      EngineState(users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops)
    }
  }
}

fn leave_sub(state: EngineState, user: String, subreddit_name: String, new_ops: Int) -> EngineState {
  let updated = remove_user_from_subreddit(state.subreddits, subreddit_name, user)
  let final_subs = filter_empty_subreddits(updated)
  let removed = get_removed_subreddits(updated, final_subs)
  let new_global = filter_posts_not_in_removed(state.global_posts, removed)
  EngineState(state.users, final_subs, state.votes, new_global, state.post_id_counter, state.comment_id_counter, new_ops)
}

fn remove_user_from_subreddit(subs: List(SubredditEntry), subreddit_name: String, user: String) -> List(SubredditEntry) {
  case subs {
    [] -> []
    [first, ..rest] -> {
      case first {
        SubredditEntry(name, subreddit) -> {
          case name == subreddit_name {
            True -> {
              let members = filter_user_from_members(subreddit.members, user)
              list.append([SubredditEntry(name, reddit_types.Subreddit(name, members, subreddit.posts))], remove_user_from_subreddit(rest, subreddit_name, user))
            }
            False -> list.append([first], remove_user_from_subreddit(rest, subreddit_name, user))
          }
        }
      }
    }
  }
}

fn filter_user_from_members(members: List(String), user: String) -> List(String) {
  case members {
    [] -> []
    [first, ..rest] -> {
      case first == user {
        True -> filter_user_from_members(rest, user)
        False -> list.append([first], filter_user_from_members(rest, user))
      }
    }
  }
}

fn filter_empty_subreddits(subs: List(SubredditEntry)) -> List(SubredditEntry) {
  case subs {
    [] -> []
    [first, ..rest] -> {
      case first {
        SubredditEntry(_, subreddit) -> {
          case subreddit.members {
            [] -> filter_empty_subreddits(rest)
            _ -> list.append([first], filter_empty_subreddits(rest))
          }
        }
      }
    }
  }
}

fn get_removed_subreddits(before: List(SubredditEntry), after: List(SubredditEntry)) -> List(String) {
  case before {
    [] -> []
    [first, ..rest] -> {
      case first {
        SubredditEntry(name, subreddit) -> {
          case subreddit.members {
            [] -> {
              case is_subreddit_in_list(after, name) {
                True -> get_removed_subreddits(rest, after)
                False -> list.append([name], get_removed_subreddits(rest, after))
              }
            }
            _ -> get_removed_subreddits(rest, after)
          }
        }
      }
    }
  }
}

fn is_subreddit_in_list(subs: List(SubredditEntry), target: String) -> Bool {
  case subs {
    [] -> False
    [first, ..rest] -> {
      case first {
        SubredditEntry(name, _) -> {
          case name == target {
            True -> True
            False -> is_subreddit_in_list(rest, target)
          }
        }
      }
    }
  }
}

fn filter_posts_not_in_removed(posts: List(reddit_types.Post), removed: List(String)) -> List(reddit_types.Post) {
  case posts {
    [] -> []
    [first, ..rest] -> {
      case first {
        reddit_types.Post(_, _, sub_name, _, _, _, _, _) -> {
          case is_string_in_list(removed, sub_name) {
            True -> filter_posts_not_in_removed(rest, removed)
            False -> list.append([first], filter_posts_not_in_removed(rest, removed))
          }
        }
      }
    }
  }
}

fn create_post(state: EngineState, author: String, subreddit_name: String, title: String, body: String, new_ops: Int) -> EngineState {
  let new_id = state.post_id_counter + 1
  let post = reddit_types.Post(new_id, author, subreddit_name, title, body, 0, [], new_ops)

  let updated_subreddits = add_post_to_subreddit(state.subreddits, subreddit_name, post)
  let exists = subreddit_exists(updated_subreddits, subreddit_name)

  let final_subreddits = case exists {
    True -> updated_subreddits
    False -> {
      let new_sub = SubredditEntry(subreddit_name, reddit_types.Subreddit(subreddit_name, [author], [post]))
      list.append(updated_subreddits, [new_sub])
    }
  }

  let new_global_posts = list.append(state.global_posts, [post])
  EngineState(state.users, final_subreddits, state.votes, new_global_posts, new_id, state.comment_id_counter, new_ops)
}

fn add_post_to_subreddit(subs: List(SubredditEntry), subreddit_name: String, post: reddit_types.Post) -> List(SubredditEntry) {
  case subs {
    [] -> []
    [first, ..rest] -> {
      case first {
        SubredditEntry(name, subreddit) -> {
          case name == subreddit_name {
            True -> {
              let posts = list.append(subreddit.posts, [post])
              list.append([SubredditEntry(name, reddit_types.Subreddit(name, subreddit.members, posts))], add_post_to_subreddit(rest, subreddit_name, post))
            }
            False -> list.append([first], add_post_to_subreddit(rest, subreddit_name, post))
          }
        }
      }
    }
  }
}

fn vote_post(state: EngineState, voter: String, post_id: Int, delta: Int, new_ops: Int) -> EngineState {
  let new_value = case delta < 0 { True -> -1 False -> case delta > 0 { True -> 1 False -> 0 } }
  let prev = get_previous_vote(state.votes, post_id, voter)
  let change = new_value - prev

  let new_subs = update_post_scores(state.subreddits, post_id, change)
  let maybe_post = find_post(new_subs, post_id)

  let new_users = case maybe_post {
    Found(p) -> {
      case p {
        reddit_types.Post(_id, author_name, _, _, _, _score, _comments, _ts) -> {
          update_user_karma(state.users, author_name, change)
        }
      }
    }
    NotFound -> state.users
  }

  let found = is_vote_exists(state.votes, post_id, voter)
  let new_votes = case found {
    True -> update_vote_value(state.votes, post_id, voter, new_value)
    False -> list.append(state.votes, [VoteEntry(post_id, voter, new_value)])
  }

  let updated_global = case maybe_post {
    Found(p) -> {
      case p {
        reddit_types.Post(id_p, _, _, _, _, _, _, _) -> {
          update_global_post_score(state.global_posts, id_p, change)
        }
      }
    }
    NotFound -> state.global_posts
  }

  EngineState(new_users, new_subs, new_votes, updated_global, state.post_id_counter, state.comment_id_counter, new_ops)
}

fn get_previous_vote(votes: List(VoteEntry), post_id: Int, voter: String) -> Int {
  case votes {
    [] -> 0
    [first, ..rest] -> {
      case first {
        VoteEntry(pid, who, value) -> {
          case pid == post_id && who == voter {
            True -> value
            False -> get_previous_vote(rest, post_id, voter)
          }
        }
      }
    }
  }
}

fn is_vote_exists(votes: List(VoteEntry), post_id: Int, voter: String) -> Bool {
  case votes {
    [] -> False
    [first, ..rest] -> {
      case first {
        VoteEntry(pid, who, _val) -> {
          case pid == post_id && who == voter {
            True -> True
            False -> is_vote_exists(rest, post_id, voter)
          }
        }
      }
    }
  }
}

fn update_vote_value(votes: List(VoteEntry), post_id: Int, voter: String, new_value: Int) -> List(VoteEntry) {
  case votes {
    [] -> []
    [first, ..rest] -> {
      case first {
        VoteEntry(pid, who, _val) -> {
          case pid == post_id && who == voter {
            True -> list.append([VoteEntry(pid, who, new_value)], update_vote_value(rest, post_id, voter, new_value))
            False -> list.append([first], update_vote_value(rest, post_id, voter, new_value))
          }
        }
      }
    }
  }
}

fn update_post_scores(subs: List(SubredditEntry), post_id: Int, change: Int) -> List(SubredditEntry) {
  case subs {
    [] -> []
    [first, ..rest] -> {
      case first {
        SubredditEntry(name, subreddit) -> {
          let new_posts = update_posts_score(subreddit.posts, post_id, change)
          list.append([SubredditEntry(name, reddit_types.Subreddit(name, subreddit.members, new_posts))], update_post_scores(rest, post_id, change))
        }
      }
    }
  }
}

fn update_posts_score(posts: List(reddit_types.Post), post_id: Int, change: Int) -> List(reddit_types.Post) {
  case posts {
    [] -> []
    [first, ..rest] -> {
      case first {
        reddit_types.Post(id, author, subreddit_name, title, body, score, comments, ts_p) -> {
          case id == post_id {
            True -> list.append([reddit_types.Post(id, author, subreddit_name, title, body, score + change, comments, ts_p)], update_posts_score(rest, post_id, change))
            False -> list.append([first], update_posts_score(rest, post_id, change))
          }
        }
      }
    }
  }
}

fn update_user_karma(users: List(UserEntry), author_name: String, change: Int) -> List(UserEntry) {
  case users {
    [] -> []
    [first, ..rest] -> {
      case first {
        UserEntry(n, u) -> {
          case n == author_name {
            True -> {
              let updated_user = reddit_types.User(u.name, u.karma + change, u.inbox, u.subreddits, u.password)
              list.append([UserEntry(n, updated_user)], update_user_karma(rest, author_name, change))
            }
            False -> list.append([first], update_user_karma(rest, author_name, change))
          }
        }
      }
    }
  }
}

fn update_global_post_score(posts: List(reddit_types.Post), post_id: Int, change: Int) -> List(reddit_types.Post) {
  case posts {
    [] -> []
    [first, ..rest] -> {
      case first {
        reddit_types.Post(id_g, author_g, sub_g, title_g, body_g, score_g, comments_g, ts_g) -> {
          case id_g == post_id {
            True -> list.append([reddit_types.Post(id_g, author_g, sub_g, title_g, body_g, score_g + change, comments_g, ts_g)], update_global_post_score(rest, post_id, change))
            False -> list.append([first], update_global_post_score(rest, post_id, change))
          }
        }
      }
    }
  }
}

// ============= ENGINE LOOP =============

fn engine_loop(state: EngineState) {
  let _ = io.println("Engine running... operations=" <> int.to_string(state.operations))
  engine_loop(state)
}

// ============= DIAGNOSTIC HELPER =============

pub fn list_subreddits(state: EngineState) -> List(reddit_types.Subreddit) {
  case state.subreddits {
    [] -> []
    _ -> extract_subreddits(state.subreddits)
  }
}

fn extract_subreddits(entries: List(SubredditEntry)) -> List(reddit_types.Subreddit) {
  case entries {
    [] -> []
    [first, ..rest] -> {
      case first {
        SubredditEntry(_, subreddit) -> list.append([subreddit], extract_subreddits(rest))
      }
    }
  }
}