import gleam/io
import gleam/list
import gleam/string
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

// Engine function result: new state plus a structured reply.
pub type EngineResult {
  EngineResult(
  EngineState,
  reddit_types.EngineReply
  )
}

// Local helper type for searching posts
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
  EngineState(list.new(), list.new(), list.new(), list.new(), 0, 0, 0)
  )
}

// Public API: handle a single message and return the new state.
pub fn handle_message(state: EngineState, msg: reddit_types.EngineMsg) -> EngineResult {
  // Each message increments the operation counter. Read-only ops still bump the counter.
  let new_ops = state.operations + 1
  case msg {
    reddit_types.Join(name) -> {
      let _ = reddit_metrics.log_event("Join: " <> name)
      let new_state = add_user(state, name, new_ops)
      EngineResult(new_state, reddit_types.Ok)
    }

    reddit_types.JoinSub(user, subreddit) -> {
      let _ = reddit_metrics.log_event("JoinSub: " <> user <> " -> " <> subreddit)
      // Ensure user exists (legacy add_user will add if missing)
      let state1 = add_user(state, user, new_ops)

      // Check if subreddit exists
      let exists = list.any(state1.subreddits, fn(e) { case e { SubredditEntry(n, _) -> n == subreddit } })
      case exists {
        False -> {
          // create subreddit with user as first member
          let new_sub = SubredditEntry(subreddit, reddit_types.Subreddit(subreddit, [user], list.new()))
          let final = list.append(state1.subreddits, [new_sub])
          // Also record subreddit in the user's data (idempotent)
          let updated_users = list.map(state1.users, fn(e) {
            case e {
              UserEntry(n, u) -> case n == user {
                True -> case list.any(u.subreddits, fn(ss) { ss == subreddit }) {
                  True -> e
                  False -> UserEntry(n, reddit_types.User(u.name, u.karma, u.inbox, list.append(u.subreddits, [subreddit]), u.password))
                }
                False -> e
              }
            }
          })
          EngineResult(EngineState(updated_users, final, state1.votes, state1.global_posts, state1.post_id_counter, state1.comment_id_counter, new_ops), reddit_types.Ok)
        }
        True -> {
          // check membership
          let already = list.any(state1.subreddits, fn(e) { case e { SubredditEntry(n, s) -> n == subreddit && list.any(s.members, fn(m) { m == user }) } })
          case already {
            True -> EngineResult(EngineState(state1.users, state1.subreddits, state1.votes, state1.global_posts, state1.post_id_counter, state1.comment_id_counter, new_ops), reddit_types.Error("already_member"))
            False -> {
              // add member to existing subreddit
              let updated = list.map(state1.subreddits, fn(entry) {
                case entry { SubredditEntry(n, s) -> case n == subreddit { True -> SubredditEntry(n, reddit_types.Subreddit(n, list.append(s.members, [user]), s.posts)) False -> entry } }
              })
              // Also update the user's subreddit list (idempotent)
              let updated_users = list.map(state1.users, fn(e) {
                case e {
                  UserEntry(n, u) -> case n == user {
                    True -> case list.any(u.subreddits, fn(ss) { ss == subreddit }) {
                      True -> e
                      False -> UserEntry(n, reddit_types.User(u.name, u.karma, u.inbox, list.append(u.subreddits, [subreddit]), u.password))
                    }
                    False -> e
                  }
                }
              })
              EngineResult(EngineState(updated_users, updated, state1.votes, state1.global_posts, state1.post_id_counter, state1.comment_id_counter, new_ops), reddit_types.Ok)
            }
          }
        }
      }
    }

    reddit_types.LeaveSub(user, subreddit) -> {
      let _ = reddit_metrics.log_event("LeaveSub: " <> user <> " -> " <> subreddit)
      // If subreddit doesn't exist, return error
      let found = list.any(state.subreddits, fn(e) { case e { SubredditEntry(n, _) -> n == subreddit } })
      case found {
        False -> EngineResult(EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops), reddit_types.Error("subreddit_not_found"))
        True -> {
          // Check membership
          let member = list.any(state.subreddits, fn(e) { case e { SubredditEntry(n, s) -> n == subreddit && list.any(s.members, fn(m) { m == user }) } })
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
      // If subreddit exists, author must be a member to post. If it doesn't exist, create it and add author as member.
      let sub_exists = list.any(state.subreddits, fn(e) { case e { SubredditEntry(n, _) -> n == subreddit } })
      case sub_exists {
        True -> {
          // find if author is member
          let is_member = list.any(state.subreddits, fn(e) { case e { SubredditEntry(n, s) -> n == subreddit && list.any(s.members, fn(m) { m == author }) } })
          case is_member {
            True -> {
              let new_state = create_post(state, author, subreddit, title, body, new_ops)
              case new_state { EngineState(_, _, _, _, post_id_counter, _, _) -> EngineResult(new_state, reddit_types.OkWithId(post_id_counter)) }
            }
            False -> {
              // user not a member
              EngineResult(EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops), reddit_types.Error("not_member"))
            }
          }
        }
        False -> {
          // create subreddit and add author as first member, then create post
          let new_sub = SubredditEntry(subreddit, reddit_types.Subreddit(subreddit, [author], list.new()))
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

    // Register/Login handlers (simple inline implementations)
    reddit_types.Register(name, password) -> {
      let _ = reddit_metrics.log_event("Register: " <> name)
      // validate name: non-empty, no spaces, reasonable length
      let has_space = string.contains(name, " ")
      let invalid = name == "" || has_space || string.length(name) > 64
      case invalid {
        True -> EngineResult(EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops), reddit_types.Error("invalid_name"))
        False -> {
          let exists = list.any(state.users, fn(e) { case e { UserEntry(n, _) -> n == name } })
          case exists {
            True -> EngineResult(EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops), reddit_types.Error("user_exists"))
            False -> {
              let user = reddit_types.User(name, 0, list.new(), list.new(), password)
              let entry = UserEntry(name, user)
              let users = list.append(state.users, [entry])
              EngineResult(EngineState(users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops), reddit_types.Ok)
            }
          }
        }
      }
    }

    reddit_types.Login(name, password) -> {
      let _ = reddit_metrics.log_event("Login: " <> name)
      let maybe = find_user(state.users, name)
      case maybe {
        UserNotFound -> EngineResult(EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops), reddit_types.Error("login_failed"))
        UserFound(u) -> case u { reddit_types.User(_n, _karma, _inbox, _subs, pw) -> case pw == password { True -> EngineResult(EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops), reddit_types.Ok) False -> EngineResult(EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops), reddit_types.Error("login_failed")) } }
      }
    }

    // Read APIs
    reddit_types.GetSubFeed(subreddit_name) -> {
      let _ = reddit_metrics.log_event("GetSubFeed: " <> subreddit_name)
      // Filter the global_posts for the requested subreddit and return newest-first
      let posts = list.filter(state.global_posts, fn(p) { case p { reddit_types.Post(_id, _author, sub_name, _t, _b, _s, _c, _ts) -> sub_name == subreddit_name } })
      let sorted = sort_posts_by_ts_desc(posts)
      let new_state = EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops)
      EngineResult(new_state, reddit_types.Posts(sorted))
    }

    // New paginated user-aware GetFeed
    reddit_types.GetFeed(user, page, page_size) -> {
      let _ = reddit_metrics.log_event("GetFeed: " <> user)
      // Find user subscriptions
      let maybe_user = find_user(state.users, user)
      case maybe_user {
        UserNotFound -> {
          let new_state = EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops)
          EngineResult(new_state, reddit_types.Error("user_not_found"))
        }
        UserFound(u) -> {
          // If user has subscriptions, filter the global post index to those subreddits; otherwise return all global posts
          let allowed = u.subreddits
          let posts_all = state.global_posts
          // Use an empty-list check instead of list.length for efficiency
          let posts = case allowed {
            [] -> posts_all
            _ -> list.filter(state.global_posts, fn(p) { case p { reddit_types.Post(_id, _author, sub_name, _t, _b, _s, _c, _ts) -> list.any(allowed, fn(a) { a == sub_name }) } })
          }
          let total = list.length(posts)
          let page_safe = case page <= 0 { True -> 1 False -> page }
          let page_size_safe = case page_size <= 0 { True -> 10 False -> page_size }
          let skip = { page_safe - 1 } * page_size_safe
          // sort posts newest-first, then paginate
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
      // Create the comment and attempt to insert it into the matching post's comments
      let new_comment_id = state.comment_id_counter + 1
      let new_comment = reddit_types.Comment(new_comment_id, author, body, 0, list.new(), new_ops)

      // Walk subreddits & posts to insert the comment
      let result = list.fold(state.subreddits, SubredditInsertAcc(list.new(), False), fn(acc, entry) {
        case acc {
          SubredditInsertAcc(acc_subs, True) -> SubredditInsertAcc(list.append(acc_subs, [entry]), True)
          SubredditInsertAcc(acc_subs, False) -> case entry {
            SubredditEntry(name, subreddit) -> {
              // process posts for this subreddit
              let post_acc = list.fold(subreddit.posts, PostInsertAcc(list.new(), False), fn(pacc, p) {
                case pacc {
                  PostInsertAcc(acc_posts, True) -> PostInsertAcc(list.append(acc_posts, [p]), True)
                  PostInsertAcc(acc_posts, False) -> case p { reddit_types.Post(id, author_p, subreddit_name_p, title_p, body_p, score_p, comments_p, ts_p) ->
                  case id == post_id {
                    True -> {
                      // Found the post: insert as top-level or nested comment
                      case parent_id == 0 {
                        True -> {
                          let new_comments = list.append(comments_p, [new_comment])
                          let new_post = reddit_types.Post(id, author_p, subreddit_name_p, title_p, body_p, score_p, new_comments, ts_p)
                          PostInsertAcc(list.append(acc_posts, [new_post]), True)
                        }
                        False -> {
                          let nested = insert_into_comments(comments_p, parent_id, new_comment)
                          case nested {
                            InsertResult(new_comments, True) -> {
                              let new_post = reddit_types.Post(id, author_p, subreddit_name_p, title_p, body_p, score_p, new_comments, ts_p)
                              PostInsertAcc(list.append(acc_posts, [new_post]), True)
                            }
                            InsertResult(_, False) -> {
                              // parent not found in this post
                              PostInsertAcc(list.append(acc_posts, [p]), False)
                            }
                          }
                        }
                      }
                    }
                    False -> PostInsertAcc(list.append(acc_posts, [p]), False)
                  }
                  }
                }
              })
              // post_acc contains new posts and a flag whether insertion happened
              case post_acc {
                PostInsertAcc(new_posts, True) -> {
                  let new_sub = SubredditEntry(name, reddit_types.Subreddit(name, subreddit.members, new_posts))
                  SubredditInsertAcc(list.append(acc_subs, [new_sub]), True)
                }
                PostInsertAcc(new_posts, False) -> {
                  let new_sub = SubredditEntry(name, reddit_types.Subreddit(name, subreddit.members, new_posts))
                  SubredditInsertAcc(list.append(acc_subs, [new_sub]), False)
                }
              }
            }
          }
        }
      })

      case result {
        SubredditInsertAcc(new_subs, True) -> {
          // Find the updated post in the new_subs and reflect it in global_posts
          let maybe_updated = case new_subs {
            [] -> NotFound
            _ -> find_post(new_subs, post_id)
          }
          let updated_global = case maybe_updated {
            Found(p) -> case p {
              reddit_types.Post(id_p, _, _, _, _, _, _, _) ->
              list.map(state.global_posts, fn(gp) {
                case gp {
                  reddit_types.Post(id_g, _, _, _, _, _, _, _) ->
                  case id_g == id_p {
                    True -> p
                    False -> gp
                  }
                }
              })
            }
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
      // Find recipient and add message to their inbox
      let new_users = list.map(state.users, fn(entry) {
        case entry {
          UserEntry(name, u) -> case name == to {
            True -> {
              let msg = reddit_types.DirectMessage(from, to, body, new_ops)
              let new_inbox = list.append(u.inbox, [msg])
              UserEntry(name, reddit_types.User(u.name, u.karma, new_inbox, u.subreddits, u.password))
            }
            False -> entry
          }
        }
      })
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

// Sort posts by timestamp descending (newest first) using insertion for simplicity.
fn insert_sorted(sorted_acc, p) {
  case sorted_acc {
    [] -> [p]
    [h, ..t] -> case h { reddit_types.Post(_, _, _, _, _, _, _, ts_h) -> case p { reddit_types.Post(_, _, _, _, _, _, _, ts_p) ->
    case ts_p >= ts_h { True -> list.append([p], sorted_acc) False -> list.append([h], insert_sorted(t, p)) }
    }
    }
  }
}

fn sort_posts_by_ts_desc(posts: List(reddit_types.Post)) -> List(reddit_types.Post) {
  list.fold(posts, list.new(), fn(acc, p) { insert_sorted(acc, p) })
}

// Find a post by id
fn find_post(subs: List(SubredditEntry), post_id: Int) -> MaybePost {
  // Fold over subreddits, and for each subreddit fold over its posts until we find a match.
  list.fold(subs, NotFound, fn(acc, entry) {
    case acc {
      Found(_) -> acc
      NotFound -> {
        case entry {
          SubredditEntry(_, subreddit) -> {
            // search posts
            let result = list.fold(subreddit.posts, NotFound, fn(acc2, p) {
              case acc2 {
                Found(_) -> acc2
                NotFound -> case p { reddit_types.Post(id, _, _, _, _, _, _, _) ->
                case id == post_id {
                  True -> Found(p)
                  False -> NotFound
                }
                }
              }
            })
            result
          }
        }
      }
    }
  })
}

// Find a user by name
fn find_user(users: List(UserEntry), name: String) -> MaybeUser {
  list.fold(users, UserNotFound, fn(acc, e) {
    case acc {
      UserFound(_) -> acc
      UserNotFound -> case e { UserEntry(n, u) -> case n == name { True -> UserFound(u) False -> UserNotFound } }
    }
  })
}

// Pagination helper implemented by folding with an index counter to avoid list destructuring quirks
fn paginate(xs: List(reddit_types.Post), skip: Int, count: Int) -> List(reddit_types.Post) {
  let end = skip + count
  let acc = list.fold(xs, PagAcc(list.new(), 0), fn(acc, x) {
    case acc {
      PagAcc(acc_list, idx) -> {
        case idx >= skip && idx < end {
          True -> PagAcc(list.append(acc_list, [x]), idx + 1)
          False -> PagAcc(acc_list, idx + 1)
        }
      }
    }
  })
  case acc { PagAcc(l, _) -> l }
}

// Insert into nested comments. Returns (new_comments, inserted_flag)
fn insert_into_comments(comments: List(reddit_types.Comment), parent_id: Int, new_comment: reddit_types.Comment) -> InsertResult {
  // Fold over comments building new list and short-circuit insertion flag
  list.fold(comments, InsertResult(list.new(), False), fn(acc, c) {
    case acc {
      InsertResult(acc_list, True) -> InsertResult(list.append(acc_list, [c]), True)
      InsertResult(acc_list, False) -> case c {
        reddit_types.Comment(id, author_c, body_c, score_c, replies_c, ts_c) -> {
          case id == parent_id {
            True -> {
              let new_replies = list.append(replies_c, [new_comment])
              InsertResult(list.append(acc_list, [reddit_types.Comment(id, author_c, body_c, score_c, new_replies, ts_c)]), True)
            }
            False -> {
              // Try to insert into nested replies
              let nested = insert_into_comments(replies_c, parent_id, new_comment)
              case nested {
                InsertResult(new_replies, True) -> InsertResult(list.append(acc_list, [reddit_types.Comment(id, author_c, body_c, score_c, new_replies, ts_c)]), True)
                InsertResult(_, False) -> InsertResult(list.append(acc_list, [c]), False)
              }
            }
          }
        }
      }
    }
  })
}

// Add a user (idempotent: will add duplicate entries if user already exists).
fn add_user(state: EngineState, name: String, new_ops: Int) -> EngineState {
  // Do not add duplicate users
  let exists = list.any(state.users, fn(e) { case e { UserEntry(n, _) -> n == name } })
  case exists {
    True -> EngineState(state.users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops)
    False -> {
      // legacy add_user: default empty password
      let user = reddit_types.User(name, 0, list.new(), list.new(), "")
      let entry = UserEntry(name, user)
      let users = list.append(state.users, [entry])
      EngineState(users, state.subreddits, state.votes, state.global_posts, state.post_id_counter, state.comment_id_counter, new_ops)
    }
  }
}

fn leave_sub(state: EngineState, user: String, subreddit_name: String, new_ops: Int) -> EngineState {
  let updated = list.map(state.subreddits, fn(entry) {
    case entry {
      SubredditEntry(name, subreddit) -> {
        case name == subreddit_name {
          True -> {
            let members = list.filter(subreddit.members, fn(m) { m != user })
            SubredditEntry(name, reddit_types.Subreddit(name, members, subreddit.posts))
          }
          False -> entry
        }
      }
    }
  })

  // FIXED: Keep only NON-EMPTY subreddits (previously kept empty ones - bug!)
  let final_subs = list.filter(updated, fn(entry) {
    case entry {
      SubredditEntry(_, subreddit) -> subreddit.members != []  // Changed from == to !=
    }
  })

  // Determine which subreddits were removed (members went to zero)
  let removed = list.fold(updated, list.new(), fn(acc, entry) {
    case entry { SubredditEntry(n, subreddit) -> case subreddit.members {
      [] -> list.append(acc, [n])
      _ -> acc
    } }
  })

  // Filter out posts that belonged to removed subreddits from global_posts
  let new_global = list.filter(state.global_posts, fn(p) {
    let reddit_types.Post(_, _, sub_name, _, _, _, _, _) = p
    let keep = !list.any(removed, fn(r) { r == sub_name })
    keep
  })

  EngineState(state.users, final_subs, state.votes, new_global, state.post_id_counter, state.comment_id_counter, new_ops)
}

fn create_post(state: EngineState, author: String, subreddit_name: String, title: String, body: String, new_ops: Int) -> EngineState {
  let new_id = state.post_id_counter + 1
  let post = reddit_types.Post(new_id, author, subreddit_name, title, body, 0, list.new(), new_ops)

  // Try to update existing subreddit posts, otherwise create the subreddit.
  let updated_subreddits = list.map(state.subreddits, fn(entry) {
    case entry {
      SubredditEntry(name, subreddit) -> {
        case name == subreddit_name {
          True -> {
            let posts = list.append(subreddit.posts, [post])
            SubredditEntry(name, reddit_types.Subreddit(name, subreddit.members, posts))
          }
          False -> entry
        }
      }
    }
  })

  // If the subreddit didn't exist, add it.
  let exists = list.any(updated_subreddits, fn(e) { case e { SubredditEntry(n, _) -> n == subreddit_name } })
  let final_subreddits =
  case exists {
    True -> updated_subreddits
    False -> {
      // When creating a subreddit due to a post, make the author a member.
      let new_sub = SubredditEntry(subreddit_name, reddit_types.Subreddit(subreddit_name, [author], [post]))
      list.append(updated_subreddits, [new_sub])
    }
  }

  // Also append the post to the global_posts list so feeds can use it directly.
  let new_global_posts = list.append(state.global_posts, [post])

  EngineState(state.users, final_subreddits, state.votes, new_global_posts, new_id, state.comment_id_counter, new_ops)
}

fn vote_post(state: EngineState, voter: String, post_id: Int, delta: Int, new_ops: Int) -> EngineState {
  // enforce vote values to be -1, 0, or 1
  let new_value = case delta < 0 { True -> -1 False -> case delta > 0 { True -> 1 False -> 0 } }

  // Find previous vote value for this (post_id, voter)
  let prev = list.fold(state.votes, 0, fn(acc, v) {
    case v { VoteEntry(pid, who, value) -> case pid == post_id && who == voter { True -> value False -> acc } }
  })

  // Compute the delta to apply to scores/karma
  let change = new_value - prev

  // Update posts with the change
  let new_subs = list.map(state.subreddits, fn(entry) {
    case entry {
      SubredditEntry(name, subreddit) -> {
        let new_posts = list.map(subreddit.posts, fn(p) {
          case p {
            reddit_types.Post(id, author, subreddit_name, title, body, score, comments, ts_p) ->
            case id == post_id {
              True -> reddit_types.Post(id, author, subreddit_name, title, body, score + change, comments, ts_p)
              False -> p
            }
          }
        })
        SubredditEntry(name, reddit_types.Subreddit(name, subreddit.members, new_posts))
      }
    }
  })

  // If we found the post, update author karma by `change`.
  let maybe_post = find_post(new_subs, post_id)
  let new_users = case maybe_post {
    Found(p) -> case p { reddit_types.Post(_id, author_name, _, _, _, _score, _comments, _ts) ->
    list.map(state.users, fn(e) {
      case e { UserEntry(n, u) -> case n == author_name { True -> UserEntry(n, reddit_types.User(u.name, u.karma + change, u.inbox, u.subreddits, u.password)) False -> e } }
    })
    }
    NotFound -> state.users
  }

  // Update votes list: replace existing entry or append
  let found = list.any(state.votes, fn(v) { case v { VoteEntry(pid, who, _val) -> pid == post_id && who == voter } })
  let new_votes = case found {
    True -> list.map(state.votes, fn(v) { case v { VoteEntry(pid, who, _val) -> case pid == post_id && who == voter { True -> VoteEntry(pid, who, new_value) False -> v } } })
    False -> list.append(state.votes, [VoteEntry(post_id, voter, new_value)])
  }

  // Also update the global_posts list to reflect the changed post score if present.
  let updated_global = case maybe_post {
    Found(p) -> case p { reddit_types.Post(id_p, _author_p, _sub_p, _t, _b, _s, _c, _ts) ->
    // map and replace matching post id
    list.map(state.global_posts, fn(gp) {
      case gp { reddit_types.Post(id_g, author_g, sub_g, title_g, body_g, score_g, comments_g, ts_g) ->
      case id_g == id_p { True -> reddit_types.Post(id_g, author_g, sub_g, title_g, body_g, score_g + change, comments_g, ts_g) False -> gp }
      }
    })
    }
    NotFound -> state.global_posts
  }

  EngineState(new_users, new_subs, new_votes, updated_global, state.post_id_counter, state.comment_id_counter, new_ops)
}

// Small engine loop example: in-process loop that would normally receive messages.
fn engine_loop(state: EngineState) {
  let _ = io.println("Engine running... operations=" <> int.to_string(state.operations))
  engine_loop(state)
}

// Diagnostic helper: list subreddits with member and post counts
pub fn list_subreddits(state: EngineState) -> List(reddit_types.Subreddit) {
  // Return the subreddit record values for diagnostics.
  list.map(state.subreddits, fn(entry) { case entry { SubredditEntry(_, subreddit) -> subreddit } })
}