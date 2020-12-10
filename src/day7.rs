use petgraph::graph::{DefaultIx, DiGraph, NodeIndex};
use std::{
    collections::HashMap,
    fs::File,
    io::{prelude::*, BufReader},
    path::Path,
};

fn read_inputs(filename: impl AsRef<Path>) -> Vec<String> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .collect()
}

fn parse_subcomponents(text: &str) -> Result<HashMap<String, u64>, regex::Error> {
    let regex = regex::Regex::new(r"(\d+)\s+((?:\S+\s*){2})\s+bag")?;
    let result: HashMap<String, u64> = regex
        .captures_iter(text)
        .map(|caps| {
            let name: String = caps[2].to_owned();
            let weight: u64 = caps[1].parse().unwrap();
            (name, weight)
        })
        .filter(|(name, _)| name != "no other")
        .collect();
    Ok(result)
}

fn add_edges(graph: &mut DiGraph<String, u64>, text: &str) {
    let name = text
        .split("bags contain")
        .next()
        .unwrap()
        .trim_end_matches(' ')
        .trim_start_matches(' ');
    let bags: Vec<_> = text
        .split("bags contain")
        .nth(1)
        .unwrap()
        .split(',')
        .map(|x| parse_subcomponents(x).unwrap())
        .flatten()
        .collect();

    let mut tail = graph.node_indices().find(|x| graph[*x] == name);
    if tail.is_none() {
        tail = Some(graph.add_node(name.to_owned()));
    }
    for (bag, weight) in bags.iter() {
        let mut node = graph.node_indices().find(|x| graph[*x] == *bag);
        if node.is_none() {
            node = Some(graph.add_node(bag.to_owned()));
        }
        graph.add_edge(node.unwrap(), tail.unwrap(), *weight);
    }
}

fn create_graph(text: &[String]) -> DiGraph<String, u64> {
    let mut graph = DiGraph::new();
    for line in text {
        add_edges(&mut graph, line);
    }
    graph
}

fn nested_bags(graph: &DiGraph<String, u64>, node: NodeIndex<DefaultIx>) -> u64 {
    use petgraph::{visit::EdgeRef, Direction};
    graph
        .edges_directed(node, Direction::Incoming)
        .map(|edge| edge.weight() * (1 + nested_bags(graph, edge.source())))
        .sum()
}

pub fn day7() -> (u64, u64) {
    use petgraph::visit::Bfs;
    let inputs = read_inputs("data/day7.txt");
    let graph = create_graph(&inputs);
    let shiny_gold = graph
        .node_indices()
        .find(|x| graph[*x] == "shiny gold")
        .unwrap();
    let ncontainers = {
        let mut visit = Bfs::new(&graph, shiny_gold);
        let mut nbags: u64 = 0;
        while visit.next(&graph).is_some() {
            nbags += 1;
        }
        nbags - 1
    };
    (ncontainers, nested_bags(&graph, shiny_gold))
}

#[cfg(test)]
mod tests {
    #[test]
    fn parse_subcomponents() {
        use std::collections::HashMap;
        assert_eq!(
            super::parse_subcomponents("1 shiny teal bag."),
            Ok(vec![("shiny teal", 1)]
                .iter()
                .map(|(a, b)| ((*a).to_owned(), *b))
                .collect::<HashMap<String, u64>>())
        );
        assert_eq!(
            super::parse_subcomponents("1 bright indigo bag, 2 dull salmon bags."),
            Ok(vec![("bright indigo", 1), ("dull salmon", 2)]
                .iter()
                .map(|(a, b)| ((*a).to_owned(), *b))
                .collect::<HashMap<String, u64>>())
        );
    }

    #[test]
    fn add_edges() {
        use petgraph::graph::DiGraph;
        let mut graph = DiGraph::<String, u64>::new();
        super::add_edges(&mut graph, "shiny purple bags contain 1 shiny teal bag.");
        let shiny_purple = graph.node_indices().find(|x| graph[*x] == "shiny purple");
        let shiny_teal = graph.node_indices().find(|x| graph[*x] == "shiny teal");
        assert_eq!(graph.node_count(), 2usize);
        assert_eq!(graph.edge_count(), 1usize);
        assert!(shiny_purple.is_some());
        assert!(shiny_teal.is_some());
        assert!(graph.contains_edge(shiny_teal.unwrap(), shiny_purple.unwrap()));

        super::add_edges(
            &mut graph,
            "shiny yellow bags contain 2 shiny teal bags, 3 wavy violet bags.",
        );
        let shiny_yellow = graph.node_indices().find(|x| graph[*x] == "shiny yellow");
        let wavy_violet = graph.node_indices().find(|x| graph[*x] == "wavy violet");
        assert_eq!(graph.node_count(), 4usize);
        assert_eq!(graph.edge_count(), 3usize);
        assert!(shiny_yellow.is_some());
        assert!(wavy_violet.is_some());
        assert!(graph.contains_edge(wavy_violet.unwrap(), shiny_yellow.unwrap()));
        assert!(graph.contains_edge(shiny_teal.unwrap(), shiny_yellow.unwrap()));
        assert!(graph.contains_edge(shiny_teal.unwrap(), shiny_purple.unwrap()));
    }
}
