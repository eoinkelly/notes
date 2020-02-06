package main

import "fmt"

// map, pointer, slice, channel, function and interface types all have `nil` for their zero value
func main() {
	// var y string            // zero value
	// ./zero_values.go:8:7: invalid operation: y == nil (mismatched types string and nil)

	var a map[string]string // zero value map
	if a == nil {
		fmt.Println("a == nil")
	}

	var b []string // zero value slice
	if b == nil {
		fmt.Println("b == nil")
	}

	// var c [2]string         // zero value array
	// ./zero_values.go:20:7: cannot convert nil to type [2]string

	if c == nil {
		fmt.Println("c == nil")
	}
}
