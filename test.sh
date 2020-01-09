set -e
# Update docs
cargo modoc
cargo +stable fmt

# Check formatting
cargo +stable fmt -- --check

# Check clippy
cargo clippy --all-features -- -D warnings 

# Check feature builds
cargo check
cargo check --all-features
cargo check --no-default-features
cargo check --no-default-features --features="format"
cargo check --no-default-features --features="encode"
cargo check --no-default-features --features="parse"

# Run tests
cargo test --all-features

# Run cargo docs and ensure the linking is working
cargo doc
