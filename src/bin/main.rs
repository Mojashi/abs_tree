use abs_tree::{ari::parse_file, ictrs::{ICTRS, ITRS}};

fn main() {
    let problem_file = "./problems/664.ari";
    let problem: ICTRS<_> = parse_file(problem_file).unwrap();
    let itrs: ITRS<u32> = problem.to_itrs().rename_symbols_to_u32();
    println!("{}", itrs);
}
