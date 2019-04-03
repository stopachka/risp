use std::io;
extern crate regex;
use regex::Regex;

fn tokenize(str: &str) -> Vec<String> {
  let token_regex = Regex::new(
    r###"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]+)"###
  ).unwrap();
  let mut res = vec![];
  for cap in token_regex.captures_iter(str) {
    res.push(String::from(&cap[1]));
  }
  res
}

fn main() {
  loop {
    println!("risp >");
    
    let mut expr = String::new();
    
    io::stdin().read_line(&mut expr)
      .expect("Failed to read line");

    print!("=> {}", tokenize(&expr).join(","));
  }
}