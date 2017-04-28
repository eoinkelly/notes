package main

import (
	"fmt"
	"os"
)

var aa string
var bb string = "hi"

func main() {
	for i, arg := range os.Args {
		fmt.Println(i, arg)
	}
}
