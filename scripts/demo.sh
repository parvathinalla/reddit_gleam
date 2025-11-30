#!/bin/bash

echo ""
echo "╔════════════════════════════════════════════════════════════╗"
echo "║         Reddit Clone - Automated Demo                     ║"
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
  -d '{"username":"alice","password":"secret123"}'
echo ""

curl -s -X POST ${SERVER_URL}/api/register \
  -H "Content-Type: application/json" \
  -d '{"username":"bob","password":"password456"}'
echo ""

echo ""
echo "=== Step 2: Logging In ==="

curl -s -X POST ${SERVER_URL}/api/login \
  -H "Content-Type: application/json" \
  -d '{"username":"alice","password":"secret123"}'
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

echo ""
echo "=== Step 4: Creating Posts ==="

curl -s -X POST ${SERVER_URL}/api/posts \
  -H "Content-Type: application/json" \
  -d '{"username":"alice","subreddit":"r/gleam","title":"Hello Gleam","body":"First post"}'
echo ""

curl -s -X POST ${SERVER_URL}/api/posts \
  -H "Content-Type: application/json" \
  -d '{"username":"alice","subreddit":"r/programming","title":"Distributed Systems","body":"Learning actors"}'
echo ""

echo ""
echo "=== Step 5: Getting Feed ==="

curl -s -X POST ${SERVER_URL}/api/feed \
  -H "Content-Type: application/json" \
  -d '{"username":"alice"}'
echo ""

echo ""
echo "╔════════════════════════════════════════════════════════════╗"
echo "║                  DEMO COMPLETED!                           ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""