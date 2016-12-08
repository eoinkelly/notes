extern crate hyper;
use hyper::client;

fn main() {
    let my_stop = "5014";
    let _buses_i_really_care_about = ["52", "58"];
    let _buses_i_care_about = ["56", "57"];

    // generate a URI that we can pass to hyper
    let uri = format!("https://www.metlink.org.nz/stop/{}/departures?more=4", my_stop);

    println!("URI is {}", uri);

    // make HTTP request and get the response body as a string
    let client = client::Client::new();
    let response = client.get("http://localhost:3000/").send().unwrap();
    // response :: hyper::Response
    assert_eq!(response.status, hyper::Ok);

}
