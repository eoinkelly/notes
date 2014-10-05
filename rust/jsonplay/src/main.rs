// I assume crates are like the gems of rust world
extern crate serialize;

// serialize = the crate
// json = a module within the crate
use serialize::json;

// #[] things are ???
// They are called attributes
// #[<trait name>, <trait name>]
#[deriving(Encodable, Decodable)]
pub struct HaikuStruct {
    // How would I instead say I wanted static strings here?
    first: String,
    second: String,
    third: String
}

fn main() {

    // "foo" makes a static string which we need to convert into an instance of
    // `collections::string::String`
    let haiku = HaikuStruct {
        first: "hi".to_string(),
        second: "htere".to_string(),
        third: "bad haiku".to_string()
    };

    // QUESTION: what does & do here?
    let encoded = json::encode(&haiku);

    println!("{}", encoded);

    struct Point {
        x: int,
        y: int
    }

    struct TuplePoint (int, int);

    let p = Point { x: 23i, y: 34i };
    let tp = TuplePoint(23i, 34i);

    println!("{}, {}", p.x, p.y);

    // let Point(newx, newy) = p; // Compiler error
    let TuplePoint(newx, newy) = tp; // Compiler error
    println!("{}, {}", newx, newy);

    // Enums and match
    enum MaybeColor {
        Color(int, int, int),
        MissingColor
    }

    // let c = Color(3i,4i,5i);
    let c = MissingColor;

    match c {
        Color(r,g,b) => println!("The color is {}, {}, {}.", r, g, b),
        MissingColor => println!("The color is missing")
    }
}
