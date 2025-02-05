use abs_tree::trs::{ari::parse_file, ictrs::ICTRS};
fn main() {
    let problem_file = std::env::args().nth(1).unwrap();


    // TODO: Timbukの、https://people.irisa.fr/Thomas.Genet/timbuk/timbuk4/experiments.html のタイムアウトしてるやつを試す
    // TODO: ループがない規則に関しては、完全に展開しちゃった方がいい
    // advantage: 675 676 677 678 679 680 683 684 746 749 750 753 754 757 786
    // cocoでは、他で解けてる40個 + 他で解けてない 15

    // TODO: 779 が証拠なしで無限ループ
    // TODO: 758, 759, 777, 780, 783, 791, 982, [787(super simpleで良い)]が閉じてるはずが、badといってしまう（これバグか？証拠発見を早く）
    // 上と同じだが、982で -(s(0()) 0()) -> s(0()) がかいだと言われる。これexactなprev取れてなくね
    let problem: ICTRS<_> = parse_file(&problem_file).unwrap();
    let res = problem.solve();
    println!("{:?}", res);
}
