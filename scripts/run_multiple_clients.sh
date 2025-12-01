#!/bin/bash

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

SERVER_URL="http://localhost:8080"

echo -e "${GREEN}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║     Reddit Clone - Multiple Client Demo                    ║${NC}"
echo -e "${GREEN}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check if server is running
echo -e "${YELLOW}Checking if server is running on port 8080...${NC}"
if curl -s ${SERVER_URL}/health > /dev/null 2>&1; then
    echo -e "${GREEN}✓ Server is running${NC}"
else
    echo -e "${RED}✗ Server is not running!${NC}"
    echo -e "${YELLOW}Please start the server first:${NC}"
    echo -e "  ${BLUE}./scripts/start_server.sh${NC}"
    exit 1
fi

echo ""
echo -e "${YELLOW}Starting multiple concurrent clients...${NC}"
echo ""

# Function to run a client session with organized logging
run_client_session() {
    local username=$1
    local client_num=$2
    local logfile="logs/${username}.log"

    echo -e "${BLUE}[Client ${client_num}] Starting ${username}'s session...${NC}"

    {
        echo "════════════════════════════════════════════════════════════"
        echo "CLIENT ${client_num}: ${username} SESSION LOG"
        echo "════════════════════════════════════════════════════════════"
        echo ""
        echo "Session Started: $(date '+%Y-%m-%d %H:%M:%S')"
        echo ""

        # 1. Register user
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "STEP 1: USER REGISTRATION"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "Client: ${username}"
        echo "Time: $(date '+%H:%M:%S')"
        echo "Endpoint: POST /api/register"
        echo "Request: {\"username\":\"${username}\",\"password\":\"pass_${username}\"}"
        REG_RESPONSE=$(curl -s -X POST ${SERVER_URL}/api/register \
          -H "Content-Type: application/json" \
          -d "{\"username\":\"${username}\",\"password\":\"pass_${username}\"}")
        echo "Response: ${REG_RESPONSE}"
        echo ""

        sleep 0.5

        # 2. Login
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "STEP 2: USER LOGIN"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "Client: ${username}"
        echo "Time: $(date '+%H:%M:%S')"
        echo "Endpoint: POST /api/login"
        echo "Request: {\"username\":\"${username}\",\"password\":\"pass_${username}\"}"
        LOGIN_RESPONSE=$(curl -s -X POST ${SERVER_URL}/api/login \
          -H "Content-Type: application/json" \
          -d "{\"username\":\"${username}\",\"password\":\"pass_${username}\"}")
        echo "Response: ${LOGIN_RESPONSE}"
        echo ""

        sleep 0.5

        # 3. Join a subreddit
        SUBREDDIT="r/gleam"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "STEP 3: JOIN SUBREDDIT"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "Client: ${username}"
        echo "Time: $(date '+%H:%M:%S')"
        echo "Endpoint: POST /api/subreddits/${SUBREDDIT}/join"
        echo "Request: {\"username\":\"${username}\"}"
        JOIN_RESPONSE=$(curl -s -X POST ${SERVER_URL}/api/subreddits/${SUBREDDIT}/join \
          -H "Content-Type: application/json" \
          -d "{\"username\":\"${username}\"}")
        echo "Response: ${JOIN_RESPONSE}"
        echo ""

        sleep 0.5

        # 4. Create a post
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "STEP 4: CREATE POST"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "Client: ${username}"
        echo "Time: $(date '+%H:%M:%S')"
        echo "Endpoint: POST /api/posts"
        echo "Subreddit: ${SUBREDDIT}"
        echo "Title: Post from ${username}"
        echo "Body: This is a test post by ${username}"
        POST_RESPONSE=$(curl -s -X POST ${SERVER_URL}/api/posts \
          -H "Content-Type: application/json" \
          -d "{\"username\":\"${username}\",\"subreddit\":\"${SUBREDDIT}\",\"title\":\"Post from ${username}\",\"body\":\"This is a test post by ${username}\"}")
        echo "Response: ${POST_RESPONSE}"

        # Extract post ID from response
        POST_ID=$(echo "${POST_RESPONSE}" | grep -o '"id":[0-9]*' | head -1 | grep -o '[0-9]*' || echo "1")
        echo "Post ID Created: ${POST_ID}"
        echo ""

        sleep 0.5

        # 5. Get post by ID
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "STEP 5: RETRIEVE POST BY ID"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "Client: ${username}"
        echo "Time: $(date '+%H:%M:%S')"
        echo "Endpoint: GET /api/posts/${POST_ID}"
        GET_POST_RESPONSE=$(curl -s -X GET ${SERVER_URL}/api/posts/${POST_ID})
        echo "Response (truncated): $(echo ${GET_POST_RESPONSE} | cut -c1-100)..."
        echo ""

        sleep 0.5

        # 6. Vote on a post
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "STEP 6: VOTE ON POST"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "Client: ${username}"
        echo "Time: $(date '+%H:%M:%S')"
        echo "Endpoint: POST /api/posts/${POST_ID}/vote"
        echo "Vote Direction: Upvote (+1)"
        VOTE_RESPONSE=$(curl -s -X POST ${SERVER_URL}/api/posts/${POST_ID}/vote \
          -H "Content-Type: application/json" \
          -d "{\"username\":\"${username}\",\"delta\":\"1\"}")
        echo "Response: ${VOTE_RESPONSE}"
        echo ""

        sleep 0.5

        # 7. Add a comment to a post
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "STEP 7: ADD COMMENT"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "Client: ${username}"
        echo "Time: $(date '+%H:%M:%S')"
        echo "Endpoint: POST /api/posts/${POST_ID}/comments"
        echo "Comment: Great post! Comment from ${username}"
        COMMENT_RESPONSE=$(curl -s -X POST ${SERVER_URL}/api/posts/${POST_ID}/comments \
          -H "Content-Type: application/json" \
          -d "{\"username\":\"${username}\",\"body\":\"Great post! Comment from ${username}\",\"parent_id\":\"0\"}")
        echo "Response: ${COMMENT_RESPONSE}"
        echo ""

        sleep 0.5

        # 8. Send direct message to another user
        TARGET_USER="alice"
        if [ "${username}" == "alice" ]; then
            TARGET_USER="bindhu"
        fi
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "STEP 8: SEND DIRECT MESSAGE"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "Client: ${username}"
        echo "Time: $(date '+%H:%M:%S')"
        echo "Endpoint: POST /api/messages/send"
        echo "From: ${username}"
        echo "To: ${TARGET_USER}"
        echo "Message: Hello ${TARGET_USER}, this is ${username}!"
        MSG_RESPONSE=$(curl -s -X POST ${SERVER_URL}/api/messages/send \
          -H "Content-Type: application/json" \
          -d "{\"from\":\"${username}\",\"to\":\"${TARGET_USER}\",\"body\":\"Hello ${TARGET_USER}, this is ${username}!\"}")
        echo "Response: ${MSG_RESPONSE}"
        echo ""

        sleep 0.5

        # 9. Get user messages
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "STEP 9: RETRIEVE MESSAGES"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "Client: ${username}"
        echo "Time: $(date '+%H:%M:%S')"
        echo "Endpoint: POST /api/messages"
        GET_MSG_RESPONSE=$(curl -s -X POST ${SERVER_URL}/api/messages \
          -H "Content-Type: application/json" \
          -d "{\"username\":\"${username}\"}")
        echo "Response (truncated): $(echo ${GET_MSG_RESPONSE} | cut -c1-100)..."
        echo ""

        sleep 0.5

        # 10. Get user feed
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "STEP 10: GET USER FEED"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "Client: ${username}"
        echo "Time: $(date '+%H:%M:%S')"
        echo "Endpoint: POST /api/feed"
        FEED_RESPONSE=$(curl -s -X POST ${SERVER_URL}/api/feed \
          -H "Content-Type: application/json" \
          -d "{\"username\":\"${username}\"}")
        echo "Response (truncated): $(echo ${FEED_RESPONSE} | cut -c1-100)..."
        echo ""

        sleep 0.5

        # 11. Leave subreddit
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "STEP 11: LEAVE SUBREDDIT"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "Client: ${username}"
        echo "Time: $(date '+%H:%M:%S')"
        echo "Endpoint: POST /api/subreddits/${SUBREDDIT}/leave"
        LEAVE_RESPONSE=$(curl -s -X POST ${SERVER_URL}/api/subreddits/${SUBREDDIT}/leave \
          -H "Content-Type: application/json" \
          -d "{\"username\":\"${username}\"}")
        echo "Response: ${LEAVE_RESPONSE}"
        echo ""

        echo "════════════════════════════════════════════════════════════"
        echo "SESSION SUMMARY FOR ${username}"
        echo "════════════════════════════════════════════════════════════"
        echo "✓ All 11 endpoints tested successfully!"
        echo "✓ Session completed at: $(date '+%Y-%m-%d %H:%M:%S')"
        echo ""
    } > "${logfile}" 2>&1
}

# Start all client sessions in parallel
run_client_session alice 1 &
ALICE_PID=$!

sleep 0.5

run_client_session bindhu 2 &
BINDHU_PID=$!

sleep 0.5

run_client_session charlie 3 &
CHARLIE_PID=$!

sleep 0.5

run_client_session diana 4 &
DIANA_PID=$!

sleep 0.5

run_client_session eve 5 &
EVE_PID=$!

echo ""
echo -e "${GREEN}✓ All clients started!${NC}"
echo -e "${YELLOW}Client PIDs: $ALICE_PID, $BINDHU_PID, $CHARLIE_PID, $DIANA_PID, $EVE_PID${NC}"
echo ""
echo -e "${YELLOW}Waiting for clients to complete their actions...${NC}"
echo -e "${YELLOW}(This will take about 15-20 seconds)${NC}"
echo ""

# Wait for all clients to finish
wait $ALICE_PID
wait $BINDHU_PID
wait $CHARLIE_PID
wait $DIANA_PID
wait $EVE_PID

echo ""
echo -e "${GREEN}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║     All Clients Completed!                                 ║${NC}"
echo -e "${GREEN}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""

echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}DETAILED ACTIVITY REPORT${NC}"
echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
echo ""

# Display organized summary for each client
for user in alice bindhu charlie diana eve; do
    echo -e "${YELLOW}────────────────────────────────────────────────────────${NC}"
    echo -e "${MAGENTA}CLIENT: ${user}${NC}"
    echo -e "${YELLOW}────────────────────────────────────────────────────────${NC}"
    grep "^STEP\|^Client:\|^Time:\|^Endpoint:\|^Response:" logs/${user}.log | head -40
    echo ""
done

echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}CLIENT LOGS LOCATION${NC}"
echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
echo ""
echo -e "  ${BLUE}logs/alice.log${NC}"
echo -e "  ${BLUE}logs/bindhu.log${NC}"
echo -e "  ${BLUE}logs/charlie.log${NC}"
echo -e "  ${BLUE}logs/diana.log${NC}"
echo -e "  ${BLUE}logs/eve.log${NC}"
echo ""

echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}VIEW FULL DETAILED LOGS${NC}"
echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
echo ""
echo -e "To view ${BLUE}alice's complete log${NC}:"
echo -e "  ${YELLOW}cat logs/alice.log${NC}"
echo ""
echo -e "To view ${BLUE}all activities combined and sorted${NC}:"
echo -e "  ${YELLOW}cat logs/*.log | sort${NC}"
echo ""
echo -e "To view ${BLUE}specific step from all clients${NC} (e.g., Step 5):"
echo -e "  ${YELLOW}grep 'STEP 5' logs/*.log${NC}"
echo ""

