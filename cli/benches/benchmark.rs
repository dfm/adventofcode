use aoc_cli::{days, download};
use criterion::{black_box, criterion_group, criterion_main, Criterion};

pub fn benchmark(c: &mut Criterion) {
    println!("{}", env!("CARGO_MANIFEST_DIR"));
    for day in 1..=days::MAX_DAY {
        if download::get_input(day).is_ok() {
            let handler = days::load_data(day).unwrap();
            c.bench_function(&format!("day {}", day), |b| {
                b.iter(|| days::run_day_with_data(black_box(day), black_box(&handler)))
            });
        }
    }
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
