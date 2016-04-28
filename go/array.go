package main

import "fmt"

func main() {
	array_examples()
}

func array_examples() {
	// There are ? ways to create an array (seclarations always include an initialization in go)

	// var things = [3]string{"hello", "there", "blah"}
	things := [3]string{"hello", "there", "blah"}

	// type signature is []<type>
	var things2 []int

	// use a range to iterate across the array data structure
	for i, thing := range things {
		fmt.Println("Thing", thing, "is at index", i)
	}
}
