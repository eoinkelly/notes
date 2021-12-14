# Logging in rust

https://crates.io/crates/log

rust provides the log crate as an abstraction over logging. You can plug different implementations into it

* libraries should just use the log crate abstraction
* executables should setup logging with their chosen implementation - https://crates.io/crates/log has links to implementations

The basic use of the log crate is through the five logging macros: error!, warn!, info!, debug! and trace! where error! represents the highest-priority log messages and trace! the lowest.

The log messages are filtered by configuring the log level to exclude messages with a lower priority. Each of these macros accept format strings similarly to println!
