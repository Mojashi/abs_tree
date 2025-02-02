use abs_tree::trs::{ari::parse_file, ictrs::ICTRS};

fn main() {
    let problem_file = std::env::args().nth(1).unwrap();

    let problem: ICTRS<_> = parse_file(&problem_file).unwrap();
    let res = problem.solve();
    println!("{:?}", res);
}
