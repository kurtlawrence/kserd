
# New
- Increase MSRV to 1.50.0
- Fix `Number::as_[u128,i128]` unsoundness issues (#29)
- Fix number parsing, preferring integer parsing (#33)
- Double float parsing performance for `Number::from_str` using the crate
    [`flast-float`](https://crates.io/crates/fast-float).

# 0.4.0
- Add `Borrow` trait to `Kstr`
- Added the container `Accessor` API
- Fix parsing of named tuples
- Fix parsing bug in containers
- Add ability to name containers using `:name` syntax
- Improve backtrace messages for some parsing errors
- Implement `FromStr` for `Number`
- Implement `Hash` for `Number`

## 0.4.1
- Increase MSRV to 1.44.1
- Update dependencies

## 0.4.2
- Increase `Number` `FromStr` performance.

# 0.3.0
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
