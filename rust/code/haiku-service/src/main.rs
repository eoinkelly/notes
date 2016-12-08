
// $ cargo build

// A web service that returns JSON over HTTP
// The JSON is an array of haiku objects
//
// Milestone 1: make a binary that will return a single hardcoded JSON haiku over HTTP

/// teepee https://github.com/teepee/teepee
/// -- still super alpha
///
/// rust-http https://github.com/chris-morgan/rust-http
/// -- deprecated in favour of teepee
/// ++ more stable
///
/// iron http://ironframework.io/
/// * middleware oriented server framework for creating apps and APIs
/// * builds on top of rust

extern crate serialize;

extern crate time;
extern crate http;

use serialize::json;
use std::io::net::ip::{SocketAddr, Ipv4Addr};
use std::io::Writer;

use http::server::{Config, Server, Request, ResponseWriter};
use http::headers::content_type::MediaType;

#[deriving(Clone)]
struct HelloWorldServer;

impl Server for HelloWorldServer {

    fn get_config(&self) -> Config {
        Config { bind_address: SocketAddr { ip: Ipv4Addr(127, 0, 0, 1), port: 8001 } }
    }

    fn handle_request(&self, _r: Request, w: &mut ResponseWriter) {
        w.headers.date = Some(time::now_utc());
        w.headers.content_length = Some(14);
        w.headers.content_type = Some(MediaType {
            type_: String::from_str("text"),
            subtype: String::from_str("plain"),
            parameters: vec!((String::from_str("charset"), String::from_str("UTF-8")))
        });
        w.headers.server = Some(String::from_str("Example"));

        w.write(b"Hello, from eoin!\n").unwrap();
    }
}

#[deriving(Decodable, Encodable)]
pub struct HaikuStruct {
    first: String,
    second: String,
    third: String
}


fn main() {
    let haiku = HaikuStruct {
        first: "first line",
        second: "second line",
        third: "third line"
    };

    let encoded = json::encode(&haiku);

    HelloWorldServer.serve_forever();
}


// JSON
// compiler provices
// #[deriving(Decodable, Encodable)]
// annotation fthat will auto generate code for the traits

