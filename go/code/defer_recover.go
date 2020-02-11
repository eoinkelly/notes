package main

import (
	"fmt"
)

func main() {
	fmt.Println("return value of c():", c())
	fmt.Println("bye bye")
}

func c() (i int) {
	defer func() {
		err := recover()
		if err != nil {
			// there is something to recover from
			fmt.Println("c() had error:", err)
			// runtime error: index out of range [2] with length 2
		} else {
			fmt.Println("c() went fine")
		}
	}()
	aa := []int{33, 44}

	fmt.Println("val from array", aa[2]) // try to get an index which doesn't exist
	return 1
}
