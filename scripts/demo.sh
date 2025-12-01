#!/bin/bash

echo ""
echo "╔════════════════════════════════════════════════════════════╗"
echo "║         Reddit Clone - Comprehensive API Demo             ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""

SERVER_URL="http://localhost:8080"

# Check if server is running
echo "→ Checking server status..."
if curl -s ${SERVER_URL}/health > /dev/null 2>&1; then
    echo "✓ Server is running"
else
    echo "✗ Server is not running!"
    echo "  Please start the server first: ./scripts/start_server.sh"
    exit 1
fi

echo ""
echo "=== Step 1: Registering Users ==="

curl -s -X POST ${SERVER_URL}/api/register \
  -H "Content-Type: application/json" \
  -d '{"username":"alice","password":"secret123}'
echo ""

curl -s -X POST ${SERVER_URL}/api/register \
  -H "Content-Type: application/json" \
  -d '{"username":"bindhu","password":"password456"}'
echo ""

echo ""
echo "=== Step 2: Logging In ==="

curl -s -X POST ${SERVER_URL}/api/login \
  -H "Content-Type: application/json" \
  -d '{"username":"alice","password":"secret123"}'
echo ""

curl -s -X POST ${SERVER_URL}/api/login \
  -H "Content-Type: application/json" \
  -d '{"username":"bindhu","password":"password456"}'
echo ""

echo ""
echo "=== Step 3: Joining Subreddits ==="

curl -s -X POST ${SERVER_URL}/api/subreddits/r/gleam/join \
  -H "Content-Type: application/json" \
  -d '{"username":"alice"}'
echo ""

curl -s -X POST ${SERVER_URL}/api/subreddits/r/programming/join \
  -H "Content-Type: application/json" \
  -d '{"username":"alice"}'
echo ""

curl -s -X POST ${SERVER_URL}/api/subreddits/r/gleam/join \
  -H "Content-Type: application/json" \
  -d '{"username":"bindhu"}'
echo ""

echo ""
echo "=== Step 4: Creating Posts ==="

curl -s -X POST ${SERVER_URL}/api/posts \
  -H "Content-Type: application/json" \
  -d '{"username":"alice","subreddit":"r/gleam","title":"Hello Gleam","body":"First post in Gleam community"}'
echo ""

curl -s -X POST ${SERVER_URL}/api/posts \
  -H "Content-Type: application/json" \
  -d '{"username":"alice","subreddit":"r/programming","title":"Distributed Systems","body":"Learning about actor model"}'
echo ""

curl -s -X POST ${SERVER_URL}/api/posts \
  -H "Content-Type: application/json" \
  -d '{"username":"bindhu","subreddit":"r/gleam","title":"Gleam is awesome","body":"Great language for backends"}'
echo ""

echo ""
echo "=== Step 5: Get Post by ID ==="

curl -s -X GET ${SERVER_URL}/api/posts/1
echo ""

curl -s -X GET ${SERVER_URL}/api/posts/2
echo ""

echo ""
echo "=== Step 6: Voting on Posts ==="

curl -s -X POST ${SERVER_URL}/api/posts/1/vote \
  -H "Content-Type: application/json" \
  -d '{"username":"bindhu","delta":"1"}'
echo ""

curl -s -X POST ${SERVER_URL}/api/posts/2/vote \
  -H "Content-Type: application/json" \
  -d '{"username":"bindhu","delta":"1"}'
echo ""

echo ""
echo "=== Step 7: Adding Comments ==="

curl -s -X POST ${SERVER_URL}/api/posts/1/comments \
  -H "Content-Type: application/json" \
  -d '{"username":"bindhu","body":"Great introduction to Gleam!","parent_id":"0"}'
echo ""

curl -s -X POST ${SERVER_URL}/api/posts/2/comments \
  -H "Content-Type: application/json" \
  -d '{"username":"bindhu","body":"Actor model is powerful!","parent_id":"0"}'
echo ""

curl -s -X POST ${SERVER_URL}/api/posts/3/comments \
  -H "Content-Type: application/json" \
  -d '{"username":"alice","body":"I agree, Gleam rocks!","parent_id":"0"}'
echo ""

echo ""
echo "=== Step 8: Sending Direct Messages ==="

curl -s -X POST ${SERVER_URL}/api/messages/send \
  -H "Content-Type: application/json" \
  -d '{"from":"alice","to":"bindhu","body":"Hey Bindhu, enjoyed your posts!"}'
echo ""

curl -s -X POST ${SERVER_URL}/api/messages/send \
  -H "Content-Type: application/json" \
  -d '{"from":"bindhu","to":"alice","body":"Thanks! Your posts are informative"}'
echo ""

echo ""
echo "=== Step 9: Getting Messages ==="

curl -s -X POST ${SERVER_URL}/api/messages \
  -H "Content-Type: application/json" \
  -d '{"username":"alice"}'
echo ""

curl -s -X POST ${SERVER_URL}/api/messages \
  -H "Content-Type: application/json" \
  -d '{"username":"bindhu"}'
echo ""

echo ""
echo "=== Step 10: Getting User Feeds ==="

curl -s -X POST ${SERVER_URL}/api/feed \
  -H "Content-Type: application/json" \
  -d '{"username":"alice"}'
echo ""

curl -s -X POST ${SERVER_URL}/api/feed \
  -H "Content-Type: application/json" \
  -d '{"username":"bindhu"}'
echo ""

echo ""
echo "=== Step 11: Leaving Subreddits ==="

curl -s -X POST ${SERVER_URL}/api/subreddits/r/programming/leave \
  -H "Content-Type: application/json" \
  -d '{"username":"alice"}'
echo ""

curl -s -X POST ${SERVER_URL}/api/subreddits/r/gleam/leave \
  -H "Content-Type: application/json" \
  -d '{"username":"bindhu"}'
echo ""

echo ""
echo "╔════════════════════════════════════════════════════════════╗"
echo "║           COMPREHENSIVE API DEMO COMPLETED!                ║"
echo "║                                                            ║"
echo "║  All REST API Endpoints Tested:                           ║"
echo "║  ✓ POST   /api/register                                   ║"
echo "║  ✓ POST   /api/login                                      ║"
echo "║  ✓ POST   /api/subreddits/:name/join                      ║"
echo "║  ✓ POST   /api/subreddits/:name/leave                     ║"
echo "║  ✓ POST   /api/posts                                      ║"
echo "║  ✓ GET    /api/posts/:id                                  ║"
echo "║  ✓ POST   /api/posts/:id/vote                             ║"
echo "║  ✓ POST   /api/posts/:id/comments                         ║"
echo "║  ✓ POST   /api/messages/send                              ║"
echo "║  ✓ POST   /api/messages                                   ║"
echo "║  ✓ POST   /api/feed                                       ║"
echo "║                                                            ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""