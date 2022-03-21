set -e
# Check formatting
cargo +stable fmt -- --check
echo "FORMATTING PASSED"

# Check clippy
cargo clippy --all-features -- -D warnings 
echo "CLIPPY PASSED"

# Check feature builds
cargo check
cargo check --all-features
cargo check --no-default-features
cargo check --no-default-features --features="format"
cargo check --no-default-features --features="encode"
cargo check --no-default-features --features="parse"
echo "BUILD FEATURE SETS PASSED"

# Run tests
cargo test --all-features
echo "TESTS PASSED"

# Run cargo docs and ensure the linking is working
cargo doc --no-deps
echo "DOCUMENTED!. DONE"
