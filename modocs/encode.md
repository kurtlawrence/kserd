[`Encoder`] and [`Decoder`] for converting between data structures and [`Kserd`].

Requires the _encode_ feature.

[`Kserd`] leverages [`serde`] to encode a data object into a `Kserd` data format and back again.
Any type that implements [`Serialize`] can be _encoded_ to a `Kserd`, and any type that implements
[`Deserialize`] can be _decoded_ back to that type from a `Kserd` object.

It is important to understand lifetime subtleties between encoding and decoding, the api
documentation on [`Encoder`] and [`Decoder`] have explanations. There is an alternate encoding
method which uses the trait [`ToKserd`] that can _consume_ the implementor object.

# Examples
Encoding can be done for any type that implements [`Serialize`].

```rust
# use kserd::*;
let data = (
    100,
    "Hello, world!",
    3.14
);

let expected = Kserd::new(Value::Tuple(
    vec![
        Kserd::new_num(100),
        Kserd::new_str("Hello, world!"),
        Kserd::new_num(3.14)
    ]
));

let kserd = Kserd::enc(&data);
assert_eq!(kserd, Ok(expected));
```

Decoding can be done for any type that implements [`Deserialize`].
```rust
# use kserd::*;
let kserd = Kserd::new(Value::Tuple(
    vec![
        Kserd::new_num(100),
        Kserd::new_str("Hello, world!"),
        Kserd::new_num(3.14)
    ]
));

let expected = (
    100,
    "Hello, world!",
    3.14
);

let r = kserd.decode::<(u32, &str, f32)>();
assert_eq!(r, Ok(expected));
```

An example of a round trip.
```rust
# use kserd::*;
let data = (
    100,
    "Hello, world!".to_string(), // see Decoder docs for lifetime subtleties
    3.14
);

let kserd = Kserd::enc(&data).unwrap();
let r = kserd.decode::<(u32, String, f32)>();
assert_eq!(r, Ok(data));
```

[`Decoder`]: crate::encode::Decoder
[`Deserialize`]: crate::encode::Deserialize
[`Encoder`]: crate::encode::Encoder
[`Kserd`]: crate::ds::Kserd
[`serde`]: serde
[`Serialize`]: crate::encode::Serialize
[`ToKserd`]: crate::ToKserd

