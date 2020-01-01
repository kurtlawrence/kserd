Format a [`Kserd`] into a human-readable string.

Requires the _format_ feature.

Formatting a [`Kserd`] can be done to easily read and understand a data structure, or be used as a
data storage format to store serialized data. The easiest way to format a `Kserd` is to use the
[`.as_str()`] and [`.as_str_with_config()`] methods. These methods use default formatting, or a
heuristic based [`FormattingConfig`] to control the display of a data structure.

For more fine control over a format, [`Formatter`] can be used to individually format `Kserd`
nodes.

For each `Kserd` node, a format can exist which displays the _identity_, along with a line
representation of the [`Value`]. There are three line representations, `Inline`, `Concise`, and
`Verbose`. `Inline` and `Concise` representations are delimited, the `Verbose` formatting takes
inspiration from TOML to reduce column widths, nesting, and break apart sequences and maps.

For example, suppose a `Kserd` is built that represents the contents of a `Cargo.toml` file.

```rust
# use kserd::*;
use kserd::fmt::*;

// we will manually construct a Kserd, but you could use Serialize
// to do this instead.
let cargotoml = Kserd::with_id(
   "my-crate",
    Value::new_cntr(vec![
        (
            "package",
            Kserd::with_id(
               "Package",
               Value::new_cntr(vec![
                    ("name", Kserd::new_str("a-crate")),
                    ("version", Kserd::new_str("0.1.0")),
                ]),
            )
            .unwrap(),
        ),
        (
            "dependencies",
            Kserd::new(Value::Seq(vec![
                Kserd::new_cntr(vec![
                    ("name", Kserd::new_str("serde")),
                    ("version", Kserd::new_str("1")),
                ]),
                Kserd::new_cntr(vec![
                    ("name", Kserd::new_str("rand")),
                    ("version", Kserd::new_str("0.5")),
                ]),
            ])),
        ),
    ]),
)
.unwrap();

// let's look at the default formatting
let s = cargotoml.as_str();

assert_eq!(
    &s,
r#"my-crate (
    dependencies = [
                       (name = "serde", version = "1")
                       (name = "rand", version = "0.5")
                   ]
    package = Package (name = "a-crate", version = "0.1.0")
)"#
);

// we can format it much more verbosely
// it becomes more readable
let config = FormattingConfig {
    width_limit: Some(0),
    ..Default::default()
};
let s = cargotoml.as_str_with_config(config);

assert_eq!(
    &s,
r#"
[[dependencies]]
    name = "serde"
    version = "1"

[[dependencies]]
    name = "rand"
    version = "0.5"

[package]
    name = "a-crate"
    version = "0.1.0"
"#
);

// maybe we want the package to be inline.

let mut fmtr = Formatter::new(&cargotoml);

fmtr.apply_config(config); // apply the config as before

// we get the index of the Package by filtering on id
fmtr.inline(
    fmtr.nodes()
        .filter(|n| n.kserd().id() == Some("Package"))
        .map(|n| n.index())
        .next()
        .unwrap(),
)
.unwrap();

let s = fmtr.write_string(String::new());

assert_eq!(
    &s,
r#"
[[dependencies]]
    name = "serde"
    version = "1"

[[dependencies]]
    name = "rand"
    version = "0.5"
package = Package (name = "a-crate", version = "0.1.0")
"#
);        
```

[`.as_str()`]: crate::Kserd::as_str
[`.as_str_with_config()`]: crate::Kserd::as_str_with_config
[`Formatter`]: Formatter
[`FormattingConfig`]: FormattingConfig
[`Kserd`]: crate::Kserd
[`Value`]: crate::Value
