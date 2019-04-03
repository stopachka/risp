use std::io;

fn main() {
  loop {
    println!("risp >");
    
    let mut expr = String::new();
    
    io::stdin().read_line(&mut expr)
      .expect("Failed to read line");

    print!("=> {}", expr);
  }
}