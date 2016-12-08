use std::io;
use std::rand;

fn main() {
    // Generate random number between 1-100
    let secret_number = (rand::random::<uint>() % 100u) + 1u;

    let max_guesses = 3u;
    let mut guesses = 0u;

    println!("## Guess a number! ##");
    println!("You have {} guesses", max_guesses);

    loop {
        if guesses >= max_guesses {
            println!("Sorry no more guesses");
            break;
        } else {
            guesses = guesses + 1;
        }

        print!("Please enter your guess: ");

        let guess = io::stdin().read_line()
                            .ok()
                            .expect("Failed to read line");

        println!("You guessed {}", guess);
        // println!("The secret is {}", secret_number);

        // let input_num: Option<uint> = from_str(guess.as_slice().trim());

        let unwrapped = match from_str(guess.as_slice().trim()) {
            Some(u) => u,
            None => {
                println!("Bad guess");
                continue;
            }
        };

        match compare(secret_number, unwrapped) {
            Less    => println!("Too small!"),
            Equal   => {
                println!("Correct!");
                println!("Goodbye.");
                break;
            },
            Greater => println!("Too big!")
        }
    }
}

fn compare(a: uint, b: uint) -> Ordering {
    if a < b { Less }
    else if a > b { Greater }
    else { Equal }
}
