use criterion::*;
use kserd::*;
use std::str::FromStr;

fn num_from_str(c: &mut Criterion) {
    c.bench_function("number from uint str", |b| {
        b.iter(|| Number::from_str(black_box("1234567890")))
    });
    c.bench_function("number from int str", |b| {
        b.iter(|| Number::from_str(black_box("-1234567890")))
    });
    c.bench_function("number from float str", |b| {
        b.iter(|| Number::from_str(black_box("-1234.56789e-12")))
    });
    c.bench_function("number from float str shorter", |b| {
        b.iter(|| Number::from_str(black_box("6800123.769")))
    });
    c.bench_function("number (err path) from err str", |b| {
        b.iter(|| Number::from_str(black_box("12454  432 afsaf")))
    });
    c.bench_function("number from uint underscroll str", |b| {
        b.iter(|| Number::from_str(black_box("1_234_567_890")))
    });
    c.bench_function("number from int underscroll str", |b| {
        b.iter(|| Number::from_str(black_box("-1_234_567_890")))
    });
}

criterion_group!(benches, num_from_str);
criterion_main!(benches);
