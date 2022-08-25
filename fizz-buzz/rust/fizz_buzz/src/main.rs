// use std::io::Write;

// fn main() -> std::io::Result<()> {
//     let mut stdout = std::io::stdout();

//     for i in (1..).take(100_000_000) {
//         // Fewer numbers will be evenly divisible by 5 so we do that test first
//         // to allow this test to fail early more often
//         if (i % 5 == 0) && (i % 3 == 0) {
//             writeln!(&mut stdout, "FizzBuzz")?;
//             continue
//         }

//         // Fewer numbers will be evenly divisible by 5 so we do that test first
//         // to allow this loop to exit earlier more often
//         if i % 5 == 0 {
//             writeln!(&mut stdout, "Buzz")?;
//             continue
//         }

//         if i % 3 == 0 {
//             writeln!(&mut stdout, "Fizz")?;
//             continue
//         }

//         writeln!(&mut stdout, "{}", i)?;
//     }

//     Ok(())
// }

fn main() {
    for i in 1..=1_000_000 {
        // Fewer numbers will be evenly divisible by 5 so we do that test first
        // to allow this test to fail early more often
        if (i % 5 == 0) && (i % 3 == 0) {
            println!("FizzBuzz");
        } else if i % 5 == 0 {
            // Fewer numbers will be evenly divisible by 5 so we do that test first
            // to allow this loop to exit earlier more often
            println!("Buzz");
        } else if i % 3 == 0 {
            println!("Fizz");
        } else {
            println!("{}", i);
        }
    }
}