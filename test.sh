set -e
# Update docs
cargo modoc
cargo +stable fmt

# Check formatting
cargo +stable fmt -- --check

# Run tests
cargo test
cargo test --all-features
cargo test --no-default-features
cargo test --no-default-features --features="format"
cargo test --no-default-features --features="encode"
cargo test --no-default-features --features="parse"

# Run cargo docs and ensure the linking is working
cargo doc
