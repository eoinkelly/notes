<?php

// # Useful PHP Patterns

// Get length of a unicode string
strlen(); // counts bytes not characters so will get it wrong on unicode stuff
$length = strlen( utf8_decode( $string ) );

// Check if a variable is empty
empty($var)

// Test if a variable is an array
is_array($a)

// Is an array empty
// count() can safely be used on variables that are not arrays or don't exist
count($a) == 0


// Escape a variable for display on a web page
// possibility 1:
	 echo esc_attr(strip_tags($var));
// possibility 2:
	echo htmlspecialchars($var)