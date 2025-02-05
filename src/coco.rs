/*
[
  {
    "problem": "665.ari",
    "results": [
      {
        "solver": "Moca",
        "result": "YES"
      }
    ]
  },
  {
    "problem": "666.ari",
    "results": [
      {
        "solver": "infChecker",
        "result": "NO"
      }
    ]
  },
  ...
]
*/

use std::{collections::HashMap, fs::File, io::BufReader};

use itertools::Itertools;

pub fn load_coco_results() -> HashMap<String, Option<bool>> {
    let filename = "./coco_inf_results_2024.json";
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let results: Vec<serde_json::Value> = serde_json::from_reader(reader).unwrap();

    let mut problem_results: HashMap<String, Option<bool>> = HashMap::new();

    for result in results {
        let problem = result["problem"].as_str().unwrap();
        let results = result["results"].as_array().unwrap();
        if results.len() == 0 {
            problem_results.insert(problem.to_string(), None);
        }
        for res in results {
            //let solver = res["solver"].as_str().unwrap();
            let result = res["result"].as_str().unwrap();
            if result == "YES" {
                problem_results.insert(problem.to_string(), Some(true));
            } else if result == "NO" {
                problem_results.insert(problem.to_string(), Some(false));
            }
        }
    }
    problem_results
}

#[test]
fn test_load_coco_results() {
    let results = load_coco_results();
    assert_eq!(results["665.ari"], Some(true));
    assert_eq!(results["666.ari"], Some(false));
    for (p,r) in results.iter().sorted().filter(|t| t.1 == &Some(true)) {
        println!("{}: {:?}", p, r);
    }
    for (p,r) in results.iter().sorted().filter(|t| t.1 == &None) {
        println!("{}: {:?}", p, r);
    }
    println!("YES results: {}", results.iter().filter(|t| t.1 == &Some(true)).count());
    println!("NO results: {}", results.iter().filter(|t| t.1 == &Some(false)).count());
    println!("UNKNOWN results: {}", results.iter().filter(|t| t.1 == &None).count());
}