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
}
