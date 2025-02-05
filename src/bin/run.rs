use abs_tree::{coco::load_coco_results, trs::ari::parse_file};
use itertools::Itertools;


fn main() {
    let coco_results = load_coco_results();

    let dir = std::fs::read_dir("./problems").unwrap();
    let files = dir.map(|f| f.unwrap().path().to_str().unwrap().to_string()).sorted().collect_vec();

    let mut results = Vec::new();
    for file in files {
        let coco_res = coco_results.get(&file).unwrap();
        let file = file;
        let problem = parse_file(&file).unwrap();

        let res = problem.solve();

        if res && coco_res == &Some(false) {
            println!("{}: {:?}", file, "ERROR");
        }

        results.push((file.to_string(), res));
    }

    for (file, res) in results {
        let res = if res {"INF"} else {"FEAS"};
        println!("{}: {:?}", file, res);
    }
}