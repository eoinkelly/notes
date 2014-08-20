
// how to loop through the things in an object:

var obj = {
	name: 'Eoin',
	age: 33,
	address: {
		street: 'Man St.',
		country: 'New Zealand'
	}
};

// Pattern 1: for..in
for (var myvar in obj) {
	// We use hasOwnProperty() to stop JS going up the prototype chain
	if (obj.hasOwnProperty(myvar)) {
		console.log(myvar);
	}
}

// Note that myvar is still available here.
console.log(myvar); // => "address"