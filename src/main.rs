use std::io;
use std::cmp::Ordering;

fn main() {
  let secret_number = 42;

  loop {
    println!("Guess the number!");

    let mut input_str = String::new();
    io::stdin().read_line(&mut input_str)
      .expect("Failed to read string");
    
    let guess: u32 = input_str.trim().parse()
      .expect("Please type a number!");

    match guess.cmp(&secret_number) {
        Ordering::Less => println!("too small!"),
        Ordering::Greater => println!("too big!"),
        Ordering::Equal => {
          println!("you got it!");
          break;
        }
    };
  }
}