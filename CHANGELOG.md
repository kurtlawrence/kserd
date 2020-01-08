
# Version
- Rename `ParseErr` to `Error`

# 0.2.0
- Remove `From<String>` impl for `ToKserdErr`
- Cleaned up error handling
- Implement `ToKserd` for tuples up to 12 length
- Implement `ToKserd` for `Box<T>`
- Implement `ToKserd` for `Option<T>`
- Implement `ToKserd` for `Result<T, E>`
- `ToKserdErr` can be converted into from other error types
- Increased code testing
