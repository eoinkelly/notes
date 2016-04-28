package main

import "fmt"

func main() {
	things := [3]string{"hello", "there", "blah"}

	for i, thing := range things {
		fmt.Println("Thing", thing, "is at index", i)
	}
}
