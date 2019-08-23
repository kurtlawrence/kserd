[![Build Status](https://travis-ci.com/kurtlawrence/kserd.svg?branch=master)](https://travis-ci.com/kurtlawrence/kserd)
[![Latest Version](https://img.shields.io/crates/v/kserd.svg)](https://crates.io/crates/kserd)
[![Rust Documentation](https://img.shields.io/badge/api-rustdoc-blue.svg)](https://docs.rs/kserd)
[![codecov](https://codecov.io/gh/kurtlawrence/kserd/branch/master/graph/badge.svg)](https://codecov.io/gh/kurtlawrence/kserd)

**K**urt's **S**elf **E**xplanatory **R**ust **D**ata.

See the [rs docs.](https://docs.rs/kserd/)
Look at progress and contribute on [github.](https://github.com/kurtlawrence/kserd)

## Introduction
`kserd` provides a data structure to represent programmatic data (mainly for Rust). Data is held in
enum variants covering the basic primitives along with nested structures such as tuples,
containers, sequences, and maps. `kserd` tries to differentiate itself by providing the data
structure as the in-memory intermediary between serialized format and a programming language's
structure. The serialized format is inspired by TOML to make `kserd` as human-readable as possible.

## Getting Started
With default features enabled, using `kserd` is a simple as importing the root which will contain
all the important items.

```rust
use kserd::*;

// Kserd and Value are the building blocks of the data.
let mut kserd = Kserd::with_id("greeting", Value::new_str("Hello")).unwrap();

// Kserd/Value are meant to be useable in their memory form
// not just simple data holding structure!
assert_eq!(kserd.str(), Some("Hello"));

// you can mutate in place as well
kserd.str_mut().map(|s| { s.push_str(", world!"); });
assert_eq!(kserd.str(), Some("Hello, world!"));

// encode data structures that implement serde::Serialize
let kserd = Kserd::enc(&vec![0, 1, 2, 4]).unwrap();

// can format the kserd into a human-readable string
println!("{}", kserd.as_str()); // should print [0, 1, 2, 4]
```

It is recommended to consult the [api documentation](crate) as there is extensive examples and
explanations.

## Features

The kserd library is feature gated, but all features are enabled by default.

| Feature  | Description | Further reading |
| -------- | ----------- | --------------- |
| _encode_ | Convert data structures to and from `Kserd`. Makes use of `serde` `Serialize` and `Deserialize` traits. | [`kserd::encode`](crate::encode) |
| _format_ | Format a `Kserd` to a human-readable string. | [`kserd::format`](crate::fmt) |
| _parse_  | Parse a string into a `Kserd`. | [`kserd::parse`](crate::parse) |

## To `1.0` Stabilistations

- [ ] stabilise `kserd_derive` crate
- [ ] extra implementations for `ToKserd`
- [ ] further testing of parsing to catch edge cases
