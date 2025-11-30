#!/bin/bash

echo "╔════════════════════════════════════════════════════════════╗"
echo "║         Reddit Clone Server - Starting Backend            ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""

# Check if we're in the right directory
if [ ! -f "gleam.toml" ]; then
    echo "✗ Error: gleam.toml not found!"
    echo "  Please run this script from the reddit_gleam directory"
    exit 1
fi

# Clean previous build
echo "→ Cleaning previous build..."
gleam clean

# Build Gleam code
echo "→ Building Gleam modules..."
gleam build

if [ $? -ne 0 ]; then
    echo "✗ Gleam build failed!"
    exit 1
fi

echo "✓ Gleam build successful"

# Gleam puts compiled modules in ebin/
GLEAM_DIR="build/dev/erlang/reddit_gleam/ebin"
# We'll put Erlang modules in the same directory
ERLANG_DIR="build/dev/erlang/reddit_gleam/ebin"

if [ ! -d "$GLEAM_DIR" ]; then
    echo "✗ Gleam build directory not found: $GLEAM_DIR"
    exit 1
fi

echo "→ Using build directory: $ERLANG_DIR"

# Compile Erlang server modules to the same directory as Gleam modules
echo ""
echo "→ Compiling Erlang server modules..."

erlc -o "$ERLANG_DIR" src/reddit_persistence.erl
if [ $? -ne 0 ]; then
    echo "✗ Failed to compile reddit_persistence.erl"
    exit 1
fi

erlc -o "$ERLANG_DIR" src/reddit_server.erl
if [ $? -ne 0 ]; then
    echo "✗ Failed to compile reddit_server.erl"
    exit 1
fi

erlc -o "$ERLANG_DIR" src/reddit_http_server.erl
if [ $? -ne 0 ]; then
    echo "✗ Failed to compile reddit_http_server.erl"
    exit 1
fi

echo "✓ All Erlang modules compiled"

# Check compiled files
echo ""
echo "→ Verifying compiled modules..."

# Check for Gleam compiled modules
if [ -f "$GLEAM_DIR/reddit_engine.beam" ]; then
    echo "  ✓ reddit_engine.beam"
else
    echo "  ✗ reddit_engine.beam (MISSING)"
    exit 1
fi

if [ -f "$GLEAM_DIR/reddit_types.beam" ]; then
    echo "  ✓ reddit_types.beam"
else
    echo "  ✗ reddit_types.beam (MISSING)"
fi

# Check for Erlang compiled modules
ERLANG_MODULES=("reddit_persistence" "reddit_server" "reddit_http_server")
for module in "${ERLANG_MODULES[@]}"; do
    if [ -f "$ERLANG_DIR/${module}.beam" ]; then
        echo "  ✓ ${module}.beam"
    else
        echo "  ✗ ${module}.beam (MISSING)"
        exit 1
    fi
done

echo ""
echo "✓ All required modules present"
echo ""
echo "╔════════════════════════════════════════════════════════════╗"
echo "║              Starting Reddit Clone Server                 ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""

# Start the server with the correct path
erl -pa "$GLEAM_DIR" \
    -eval "reddit_http_server:start()." \
    -noshell