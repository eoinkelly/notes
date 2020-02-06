package main

import (
	"fmt"
	"reflect"
)

type car struct {
	name      string `src:"blahsrc" reliability:"22" blank:""`
	numWheels int    `src:"vehicle"`
}

func main() {
	ferarri := car{"Ferarri", 4}

	fmt.Println(ferarri)

	t := reflect.TypeOf(car{})            // get the type
	nameField, _ := t.FieldByName("name") // get a field within the type

	// access the tag on the field
	fmt.Println(nameField.Tag)

	// Lookup returns the value and a boolean indicating whether there was a value
	val, exists := nameField.Tag.Lookup("src")
	fmt.Println(val, exists)

	// Get is like lookup but discards the boolean
	fmt.Println(nameField.Tag.Get("src"))
}
