package main

import "fmt"

func main() {
	arrayExamples()
	arrayLengthCannotBeSetWithVar()
	arraysAreMutable()
}

func arrayLengthCannotBeSetWithVar() {
	const aa = 4
	bb := 4

	var ex1 [aa]string

	// var ex2 [bb]string
	// ./array.go:27:10: non-constant array bound bb

	fmt.Println(len(ex1), bb) // just to avoid compiler warning about unused vars
}

func arrayExamples() {
	// There are ? ways to create an array (declarations always include an initialization in go)

	// var things = [3]string{"hello", "there", "blah"}
	// type signature is []<type>
	things := [3]string{"hello", "there", "blah"}

	// use a range to iterate across the array data structure
	for i, thing := range things {
		fmt.Println("Thing", thing, "is at index", i)
	}
}

func arraysAreMutable() {
	ex_3 := []int{1, 3, 4, 5, 6}

	fmt.Println(ex_3) // avoid unused vars errors

	ex_3[0] = 11

	fmt.Println(ex_3) // avoid unused vars errors
}
