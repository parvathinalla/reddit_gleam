# Reddit Clone - Part 2 Implementation

## Overview
Part 2 focuses on **comprehensive REST API testing**, **enhanced output formatting**, and **production-ready deployment**. This document details all additions and improvements made in Part 2.

## Team Members
Bindhu Sree Reddy Alla (UFID: 54455430)
Parvati Nalla (UFID: 80911450)
---

## Part 2 Highlights

### ✅ **100% REST API Endpoint Coverage**
All 11 endpoints now thoroughly tested with concurrent and sequential testing modes.

### ✅ **Enhanced Output Formatting**
Organized, detailed activity reports showing exactly which client performed which action.

### ✅ **Multiple Testing Modes**
- Concurrent testing (5 clients simultaneously)
- Sequential demo mode
- Interactive CLI client
- Multi-window support

### ✅ **Complete Documentation**
Comprehensive guides for testing, endpoint details, and terminal management.

---

## Getting Started

### Prerequisites
- Gleam installed
- Erlang/OTP runtime
- macOS (for terminal splitting recommendations)
- curl (for REST API testing)

### Quick Start

#### 1. **Start the Server**
```bash
cd /Users/bindhu/Desktop/Bindhu/Projects/reddit_gleam
./scripts/start_server.sh
```

**Expected Output:**
```
╔════════════════════════════════════════════════════════════╗
║       Reddit Clone REST API Server v2.0                   ║
╚════════════════════════════════════════════════════════════╝

Starting Reddit engine...
✓ Engine started successfully (PID: <0.82.0>)
✓ HTTP server started on http://localhost:8080

Available REST API endpoints:
  POST   /api/register         - Register new user
  POST   /api/login            - Login user
  POST   /api/subreddits/:name/join   - Join subreddit
  POST   /api/subreddits/:name/leave  - Leave subreddit
  POST   /api/posts            - Create new post
  GET    /api/posts/:id        - Get post by ID
  POST   /api/posts/:id/vote   - Vote on post
  POST   /api/posts/:id/comments - Add comment
  POST   /api/messages/send    - Send direct message
  POST   /api/messages         - Get user messages
  POST   /api/feed             - Get user feed

Server ready! Waiting for client connections...
```

**Server stays running in this terminal. Open another terminal for client.**

---

#### 2. **Run the Client (Interactive Mode)**
In a **new terminal window**, run:
```bash
cd /Users/bindhu/Desktop/Bindhu/Projects/reddit_gleam
./scripts/start_client.sh
```

**Or directly:**
```bash
gleam run -m reddit_cli_client
```

**Expected Output:**
```
╔════════════════════════════════════════════════════════════╗
║       Reddit Clone CLI Client v2.0                        ║
╚════════════════════════════════════════════════════════════╝

Make sure the server is running on http://localhost:8080

Commands:
  1 - Register user
  2 - Login user
  3 - Join subreddit
  4 - Create post
  5 - Get feed
  d - Run demo (automated)
  q - Quit

[Not logged in] Choice: 
```

**Interactive Menu Options:**
- `1` - Register a new user
- `2` - Login to your account
- `3` - Join a subreddit
- `4` - Create a post
- `5` - View your feed
- `d` - Run automated demo (tests all 11 endpoints)
- `q` - Quit

---

#### 3. **Run Sequential Demo (All Endpoints)**
In a **new terminal window**:
```bash
./scripts/demo.sh
```

**What it does:**
- Registers 2 users (alice & bindhu)
- Logs them in
- Joins multiple subreddits
- Creates posts
- Retrieves posts by ID
- Votes on posts
- Adds comments
- Sends messages
- Gets messages
- Retrieves feeds
- Leaves subreddits

**Expected Output:**
```
╔════════════════════════════════════════════════════════════╗
║         Reddit Clone - Comprehensive API Demo             ║
╚════════════════════════════════════════════════════════════╝

→ Checking server status...
✓ Server is running

=== Step 1: Registering Users ===
[Requests and responses for all endpoints...]

╔════════════════════════════════════════════════════════════╗
║           COMPREHENSIVE API DEMO COMPLETED!                ║
║  All REST API Endpoints Tested:                           ║
║  ✓ POST   /api/register                                   ║
║  ✓ POST   /api/login                                      ║
║  ✓ POST   /api/subreddits/:name/join                      ║
║  ✓ POST   /api/subreddits/:name/leave                     ║
║  ✓ POST   /api/posts                                      ║
║  ✓ GET    /api/posts/:id                                  ║
║  ✓ POST   /api/posts/:id/vote                             ║
║  ✓ POST   /api/posts/:id/comments                         ║
║  ✓ POST   /api/messages/send                              ║
║  ✓ POST   /api/messages                                   ║
║  ✓ POST   /api/feed                                       ║
╚════════════════════════════════════════════════════════════╝
```

---

#### 4. **Run Concurrent Multi-Client Test**
In a **new terminal window**:
```bash
./scripts/run_multiple_clients.sh
```

**What it does:**
- Starts 5 concurrent clients (alice, bindhu, charlie, diana, eve)
- Each client tests all 11 endpoints
- Generates organized activity reports
- Creates detailed log files for each client
- Shows which client performed each action

**Expected Output:**
```
╔════════════════════════════════════════════════════════════╗
║     Reddit Clone - Multiple Client Demo                   ║
╚════════════════════════════════════════════════════════════╝

Checking if server is running on port 8080...
✓ Server is running

Starting multiple concurrent clients...

[Client 1] Starting alice's session...
[Client 2] Starting bindhu's session...
[Client 3] Starting charlie's session...
[Client 4] Starting diana's session...
[Client 5] Starting eve's session...

✓ All clients started!
Client PIDs: 63903, 63912, 63923, 63938, 63962

Waiting for clients to complete their actions...
(This will take about 15-20 seconds)

╔════════════════════════════════════════════════════════════╗
║     All Clients Completed!                                 ║
╚════════════════════════════════════════════════════════════╝

═══════════════════════════════════════════════════════════
DETAILED ACTIVITY REPORT
═══════════════════════════════════════════════════════════

────────────────────────────────────────────────────────
CLIENT: alice
────────────────────────────────────────────────────────
STEP 1: USER REGISTRATION
Client: alice
Time: 14:23:45
Endpoint: POST /api/register
Response: {"status":"success","username":"alice"}

STEP 2: USER LOGIN
Client: alice
Time: 14:23:46
Endpoint: POST /api/login
Response: {"status":"success","token":"...","username":"alice"}

[... continues for all 11 steps and all 5 clients ...]
```

---

## Testing Modes Explained

### 1. **Interactive Client (`start_client.sh`)**
- **Best for**: Manual exploration, learning, testing individual endpoints
- **Duration**: As long as you want
- **Clients**: 1 user at a time
- **Output**: Interactive menu, real-time responses

**Commands:**
```bash
./scripts/start_client.sh
```

### 2. **Sequential Demo (`demo.sh`)**
- **Best for**: Seeing all endpoints in action, understanding the flow
- **Duration**: 5-10 seconds
- **Clients**: 2 users (alice & bindhu)
- **Output**: Organized step-by-step demonstration

**Commands:**
```bash
./scripts/demo.sh
```

### 3. **Concurrent Testing (`run_multiple_clients.sh`)**
- **Best for**: Stress testing, verifying concurrent operations, comprehensive validation
- **Duration**: 15-20 seconds
- **Clients**: 5 simultaneous users
- **Output**: Detailed organized activity report

**Commands:**
```bash
./scripts/run_multiple_clients.sh
```

---

## All 11 REST API Endpoints

### 1. **POST /api/register** - Register New User
```bash
curl -X POST http://localhost:8080/api/register \
  -H "Content-Type: application/json" \
  -d '{"username":"alice","password":"secret123"}'
```
**Response:** `{"status":"success","username":"alice"}`

### 2. **POST /api/login** - Login User
```bash
curl -X POST http://localhost:8080/api/login \
  -H "Content-Type: application/json" \
  -d '{"username":"alice","password":"secret123"}'
```
**Response:** `{"status":"success","token":"...","username":"alice"}`

### 3. **POST /api/subreddits/:name/join** - Join Subreddit
```bash
curl -X POST http://localhost:8080/api/subreddits/r/gleam/join \
  -H "Content-Type: application/json" \
  -d '{"username":"alice"}'
```
**Response:** `{"status":"success","message":"Joined r/gleam"}`

### 4. **POST /api/subreddits/:name/leave** - Leave Subreddit
```bash
curl -X POST http://localhost:8080/api/subreddits/r/gleam/leave \
  -H "Content-Type: application/json" \
  -d '{"username":"alice"}'
```
**Response:** `{"status":"success","message":"Left r/gleam"}`

### 5. **POST /api/posts** - Create New Post
```bash
curl -X POST http://localhost:8080/api/posts \
  -H "Content-Type: application/json" \
  -d '{"username":"alice","subreddit":"r/gleam","title":"Hello","body":"First post"}'
```
**Response:** `{"status":"success","post_id":1}`

### 6. **GET /api/posts/:id** - Get Post by ID
```bash
curl -X GET http://localhost:8080/api/posts/1
```
**Response:** `{"id":1,"author":"alice","subreddit":"r/gleam","title":"Hello",...}`

### 7. **POST /api/posts/:id/vote** - Vote on Post
```bash
curl -X POST http://localhost:8080/api/posts/1/vote \
  -H "Content-Type: application/json" \
  -d '{"username":"bindhu","delta":"1"}'
```
**Response:** `{"status":"success","new_score":1}`

### 8. **POST /api/posts/:id/comments** - Add Comment
```bash
curl -X POST http://localhost:8080/api/posts/1/comments \
  -H "Content-Type: application/json" \
  -d '{"username":"bindhu","body":"Great post!","parent_id":"0"}'
```
**Response:** `{"status":"success","comment_id":1}`

### 9. **POST /api/messages/send** - Send Direct Message
```bash
curl -X POST http://localhost:8080/api/messages/send \
  -H "Content-Type: application/json" \
  -d '{"from":"alice","to":"bindhu","body":"Hello!"}'
```
**Response:** `{"status":"success","message":"Message sent"}`

### 10. **POST /api/messages** - Get User Messages
```bash
curl -X POST http://localhost:8080/api/messages \
  -H "Content-Type: application/json" \
  -d '{"username":"alice"}'
```
**Response:** `{"messages":[...],"total":5}`

### 11. **POST /api/feed** - Get User Feed
```bash
curl -X POST http://localhost:8080/api/feed \
  -H "Content-Type: application/json" \
  -d '{"username":"alice"}'
```
**Response:** `{"posts":[...],"page":1,"total":10,"page_size":10}`

---

## Log Files & Output

### View Individual Client Logs
```bash
cat logs/alice.log
cat logs/bindhu.log
cat logs/charlie.log
cat logs/diana.log
cat logs/eve.log
```

### View All Activities Combined
```bash
cat logs/*.log | sort
```

### View Specific Step from All Clients
```bash
grep 'STEP 5' logs/*.log
```

### View All Endpoints Called
```bash
grep 'Endpoint:' logs/*.log
```

---

## Running Multiple Commands Side-by-Side

### **Using Two Terminal Windows (Easiest)**

**Window 1 - Server:**
```bash
./scripts/start_server.sh
```

**Window 2 - Client or Demo:**
```bash
./scripts/start_client.sh
# Or:
./scripts/demo.sh
# Or:
./scripts/run_multiple_clients.sh
```

**Arrange windows side by side:**
- Drag first window to **LEFT** side
- Drag second window to **RIGHT** side
- Resize them so both are visible

### **Using iTerm2 (Advanced)**
```bash
# Open iTerm2
open -a iTerm

# Or install it first:
brew install iterm2
```

**Then:**
1. Start server: `./scripts/start_server.sh`
2. Split vertically: `Cmd + D`
3. Start client: `./scripts/start_client.sh`

---

## Enhanced Output Format

The `run_multiple_clients.sh` script now provides **organized, detailed activity reports** instead of basic logs.

### Example Output Structure:
```
═══════════════════════════════════════════════════════════
DETAILED ACTIVITY REPORT
═══════════════════════════════════════════════════════════

────────────────────────────────────────────────────────
CLIENT: alice
────────────────────────────────────────────────────────
STEP 1: USER REGISTRATION
Client: alice
Time: 14:23:45
Endpoint: POST /api/register
Response: {"status":"success","username":"alice"}

STEP 2: USER LOGIN
Client: alice
Time: 14:23:46
Endpoint: POST /api/login
Response: {"status":"success","token":"..."}

[... continues for all 11 steps ...]

────────────────────────────────────────────────────────
CLIENT: bindhu
────────────────────────────────────────────────────────
[... similar detailed breakdown ...]
```

**Benefits:**
- ✅ See exactly which client did what
- ✅ Timestamps for each operation
- ✅ Full endpoint and response details
- ✅ Easy to trace concurrent operations
- ✅ Perfect for debugging

---

## Implementation Details

### Part 2 Changes

#### 1. **Complete REST API Testing**
- **Before**: Limited endpoint testing (5/11)
- **After**: Full coverage (11/11 endpoints)
- **Added endpoints**:
  - GET /api/posts/:id (retrieve posts)
  - POST /api/posts/:id/vote (voting)
  - POST /api/posts/:id/comments (comments)
  - POST /api/messages/send (messaging)
  - POST /api/messages (retrieve messages)
  - POST /api/subreddits/:name/leave (leave subreddit)

#### 2. **Enhanced Testing Scripts**
- **`demo.sh`**: Updated with all 11 endpoints
- **`run_multiple_clients.sh`**: Complete rewrite with organized output
- **`reddit_cli_client.gleam`**: Extended demo with full endpoint coverage

#### 3. **Output Formatting Improvements**
- Organized by client with clear visual separators
- All 11 steps numbered and labeled
- Detailed request/response information
- Timestamps for each operation
- Color-coded for easy reading

#### 4. **Fixed Issues**
- Fixed `find_user` function: Changed from flawed fold-based to proper recursive implementation
- Replaced all `list.new()` with `[]` for proper Gleam compilation
- Removed unused imports
- Enhanced error handling and logging

#### 5. **Bug Fixes**
- ✅ Login now works correctly (user lookup was broken)
- ✅ Password comparison works properly
- ✅ Empty list creation fixed
- ✅ All 5 concurrent clients complete successfully

---

## Feature Summary

### Testing Capabilities
| Feature | Status |
|---------|--------|
| REST API Endpoints | ✅ 11/11 (100%) |
| Concurrent Clients | ✅ 5 simultaneous |
| Sequential Demo | ✅ Full walkthrough |
| Interactive Client | ✅ Manual testing |
| Log Files | ✅ Per-client logging |
| Activity Reports | ✅ Organized output |

### User Operations
| Operation | Status |
|-----------|--------|
| Register | ✅ Working |
| Login | ✅ Fixed & Working |
| Join Subreddit | ✅ Working |
| Leave Subreddit | ✅ Working |
| Create Post | ✅ Working |
| Get Post | ✅ Working |
| Vote on Post | ✅ Working |
| Comment | ✅ Working |
| Send Message | ✅ Working |
| Get Messages | ✅ Working |
| Get Feed | ✅ Working |

---

## File Structure

```
reddit_gleam/
├── scripts/
│   ├── start_server.sh          # Start the REST API server
│   ├── start_client.sh          # Start interactive client
│   ├── demo.sh                  # Run sequential demo (all endpoints)
│   └── run_multiple_clients.sh  # Run concurrent test (5 clients)
├── src/
│   ├── reddit_engine.gleam      # Core engine with all operations
│   ├── reddit_cli_client.gleam  # Interactive CLI client
│   ├── reddit_types.gleam       # Type definitions
│   ├── reddit_server.erl        # Erlang server loop
│   ├── reddit_http_server.erl   # HTTP REST API endpoints
│   └── [other modules]
├── logs/                         # Generated during testing
│   ├── alice.log
│   ├── bindhu.log
│   ├── charlie.log
│   ├── diana.log
│   └── eve.log
└── README-Part-2.md             # This file
```

---

## Testing Workflow

### Complete End-to-End Test (20 minutes)

**Terminal 1 - Server:**
```bash
./scripts/start_server.sh
```
Wait for: `Server ready! Waiting for client connections...`

**Terminal 2 - Client:**
```bash
./scripts/start_client.sh
```
Then use menu (1-5, d, q)

**Terminal 3 - Demo:**
```bash
./scripts/demo.sh
```
Shows all endpoints in action

**Terminal 4 - Stress Test:**
```bash
./scripts/run_multiple_clients.sh
```
5 clients simultaneously

---

## Key Improvements in Part 2

1. ✅ **100% API Endpoint Coverage** - All 11 endpoints tested
2. ✅ **Concurrent Testing** - 5 clients running simultaneously
3. ✅ **Organized Output** - Clear activity reports per client
4. ✅ **Fixed Login** - User lookup now works correctly
5. ✅ **Better Debugging** - Full request/response visibility
6. ✅ **Multiple Test Modes** - Interactive, sequential, concurrent
7. ✅ **Production Ready** - Error handling and logging improved

---

## Troubleshooting

### Server won't start
```bash
# Check if port 8080 is in use
lsof -i :8080

# Kill any process using it
kill -9 <PID>

# Try again
./scripts/start_server.sh
```

### Client won't connect
```bash
# Make sure server is running
curl http://localhost:8080/health

# If not, start it
./scripts/start_server.sh
```

### Logs directory missing
```bash
mkdir -p logs
./scripts/run_multiple_clients.sh
```

---

## Next Steps

1. **Try the interactive client** - Manually test endpoints
2. **Run the demo** - See all operations in sequence
3. **Run concurrent tests** - Verify concurrent handling
4. **Review logs** - Understand detailed activity flow

---

## Summary

Part 2 delivers a **complete, production-ready REST API testing suite** with:
- ✅ All 11 endpoints thoroughly tested
- ✅ Multiple testing modes (interactive, sequential, concurrent)
- ✅ Organized, detailed output reports
- ✅ 5 concurrent clients simultaneously
- ✅ Fixed login functionality
- ✅ Comprehensive logging and debugging

**The Reddit Clone system is now fully validated and ready for deployment!** 🎉

---

**Created:** December 1, 2025  
**Status:** ✅ Complete - 100% API Coverage  
**Test Coverage:** 11/11 Endpoints (100%)

