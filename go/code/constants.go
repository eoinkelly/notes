package main

import "fmt"

// this value will not fit in an int64 but this code compiles just fine because constants have a precision of 256 bits
const xx = 12319230384039398938749020948903034004982382334344248338377394383

// this will not compile
// const xxNope int = 12319230384039398938749020948903034004982382334344248338377394383

// `iota` is a special value which starts at 0 and can be used when declaring a
// block of related constants. It will automatically increment and be assigned
// to the constants that follow even if they aren't explicitly assigned to it.
//
// It is useful when you want to make an enum alike thing I guess
const (
	AA = iota // 0
	BB        // 1
	CC        // 2
)

func main() {
	fmt.Printf("%d\n", AA)
	fmt.Printf("%d\n", BB)
	fmt.Printf("%d\n", CC)
}
