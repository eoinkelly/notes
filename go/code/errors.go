package main

import (
	"fmt"
	"errors"
)

type BaseError struct {
	Reason string
}

// adding this method implements the `error` interface
func (e BaseError) Error() string  {
	return fmt.Sprint("BASE PROBLEM: ", e.Reason)
}

type CustomError struct {
	Underlying BaseError
	Reason string
}

// adding this method implements the `error` interface
func (e CustomError) Error() string  {
	return fmt.Sprint("BASE PROBLEM: ", e.Reason)
}

func (e CustomError) Unwrap() error {
	return e.Underlying
}

func main() {
	e1 := errors.New("xxx")
	e2 := errors.New("xxx")

	if e1 == e2 {
		fmt.Println("e1 == e2")
	} else {
		fmt.Println("e1 != e2")
	}

	if errors.Is(e1, e2) {
		fmt.Println("e1 is e2")
	} else {
		fmt.Println("e1 is not e2")
	}

	b1 := BaseError{"who knows"}
	b2 := BaseError{"something else"}
	fmt.Println(b1)
	c1 := CustomError{Underlying: b1, Reason: "a custom fuckup" }
	fmt.Println(c1)

	if errors.Is(c1, b1) { // true
		fmt.Println("c1 is b1")
	} else {
		fmt.Println("c1 is not b1")
	}

	if errors.Is(c1, b2) { // true
		fmt.Println("c1 is b2")
	} else {
		fmt.Println("c1 is not b2")
	}

}