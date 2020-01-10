
# Version
- Rename `ParseErr` to `Error`
- Rename internal items to make clippy happy
- Rename `to_owned` to `into_owned`

# 0.2.0
- Remove `From<String>` impl for `ToKserdErr`
- Cleaned up error handling
- Implement `ToKserd` for tuples up to 12 length
- Implement `ToKserd` for `Box<T>`
- Implement `ToKserd` for `Option<T>`
- Implement `ToKserd` for `Result<T, E>`
- `ToKserdErr` can be converted into from other error types
- Increased code testing
