package main

// QUESTION: is it one package per file?

// import some packages
import (
	"fmt"
)

// Person Struct
// *************

// note: uppercase type name and member names
type Person struct {
	Name string
	Age  int
}

// associates a function with a struct using special syntax
// func <receiver> <name>(<args>) <return value> {
func (p *Person) PrettyPrint() {
	// note: println auto adds space between args
	println(p.Name, "is", p.Age, "years old")
}

func (p *Person) fly() {
	println(p.Name, "Falling to death...")
}

// Superhero struct
// ****************

// Superhero mixes in Person
// => Superhero has access to Person functions and data
type Superhero struct {
	*Person
	Superpower string
}

func (s *Superhero) fly() {
	// note: Superhero struct instance can access the mixed in Person.Name
	//       directly as Name
	println(s.Name, "soaring like a bird ...")
}

// program entry point
// *******************

func main() {
	sep()

	// var <var-name> <type> = <initial value>
	var message string = "hi from message"
	println(message)
	sep()

	// declare and initialize an integer variable
	var power_2 int = 9000
	println(power_2)
	sep()

	// declare and initialize iwth shorthand and type inference
	power_1 := 9000
	println(power_1)
	sep()

	f1, f2, f3 := "hi", 33, "blah"
	println(f1)
	println(f2)
	println(f3)
	sep()

	fmt.Println("Welcome to go")
	sep()

	r1, r2, _ := multi_return(4, 55)
	// use _ to ignore the final return value
	println(r1)
	println(r2)
	sep()

	// note the : between member name and value
	eoin_1 := Person{}
	eoin_2 := Person{
		Name: "Eoin Kelly",
		Age:  36, // note trailing , required!
	}
	eoin_3 := Person{"Eoin K", 33} // initialized fields based on order

	// unassigned values are "zero" initialized
	println(eoin_1.Name)
	println(eoin_1.Age)

	eoin_2.PrettyPrint()
	eoin_3.PrettyPrint()

	// initialize a pointer to the eoin_2 struct
	nearly_eoin := &eoin_2

	make_older(nearly_eoin)

	// print mutated eoin_2
	println(eoin_2.Age)
	sep()

	// note that that create the embedded struct you have to do it explicitly
	// even though batgirl.Name will work
	batgirl := &Superhero{
		Person: &Person{
			Name: "Bat girl",
			Age:  33,
		},
		Superpower: "being a bat",
	}

	eoin_2.fly()
	batgirl.fly()
	sep()

	// both of these work
	println(batgirl.Name)
	println(batgirl.Person.Name)
	sep()

	// declare array
	var ages_1 [3]int

	// initialize array shorthand
	// <name> := [<lenght>]<type>{<initial values>}
	ages_2 := [3]int{12, 44, 56}

	// get length of array
	println(len(ages_1))
	sep()

	// iterate across an array
	// note: this line is special syntax
	for age, index := range ages_2 {
		println(index, age)
	}
	sep()
}

func make_older(p *Person) {
	p.Age += 10
}

func sep() {
	println("**************")
}

// functions can return multiple values
func multi_return(a int, b int) (int, bool, string) {
	// func multi_return(a, b int) (int, bool, string) {
	// shorthand because a and b have same type
	result := a + b
	happy := true
	message := "hello"
	return result, happy, message
}
