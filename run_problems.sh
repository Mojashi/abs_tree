#!/bin/bash

# Directory containing the problem files
PROBLEMS_DIR="./problems"

# Export the cargo command as a function
run_cargo() {
    timeout 30s ./target/release/main "$1" --release
}

export -f run_cargo

find "$PROBLEMS_DIR" -type f | parallel -j 5 --results ./logs run_cargo {}