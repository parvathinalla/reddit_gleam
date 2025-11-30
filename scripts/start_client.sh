#!/bin/bash

echo "╔════════════════════════════════════════════════════════════╗"
echo "║         Reddit Clone Client - Starting Frontend           ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""

# Check if server is running
echo "→ Checking if server is running..."
if curl -s http://localhost:8080/health > /dev/null 2>&1; then
    echo "✓ Server is running"
else
    echo "✗ Server is not running!"
    echo "  Please start the server first using: ./scripts/start_server.sh"
    exit 1
fi

# Build client
echo ""
echo "→ Building client..."
gleam build

if [ $? -ne 0 ]; then
    echo "✗ Gleam build failed!"
    exit 1
fi

# Compile HTTP client helper
echo "→ Compiling HTTP client..."
erlc -o build/dev/erlang/reddit_gleam/ebin src/reddit_http_client.erl

if [ $? -ne 0 ]; then
    echo "✗ Failed to compile reddit_http_client.erl"
    exit 1
fi

echo "✓ Client build successful"
echo ""
echo "╔════════════════════════════════════════════════════════════╗"
echo "║              Starting Reddit Clone Client                 ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""

# Start the client
gleam run -m reddit_cli_client