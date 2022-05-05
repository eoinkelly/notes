<?php
declare(strict_types=1); // turn off coercions

// run the composer autoloader
require __DIR__ . '/vendor/autoload.php';

use Eoin\Util\Car;
use Eoin\Util\Simple;
use Eoin\Util\Dog;
use Eoin\Util\Misc;

// *one namespace per file is typical but you can have more
// * namespace must be declared before other PHP statements (except declare())
// namespace Eoin;

// logs errors to stderr which nginx then puts in log/error.log
error_log("hello 32343");

echo "<code><pre>";


$car = new Car();
$car->drive();

// ********************************

Misc::printSep();

var_dump(Misc::sum(1, 2));
// var_dump(sum(1.5, 2.5)); // throws error because we have declared strict_types


$thing_1 = new Simple('Eoin');
$thing_2 = new Simple();

$thing_1->greet();
$thing_2->greet();


$muttly = new Dog();
echo "{$muttly->makeNoise()}\n";

// ***************************************

$people = array(
    'John', 'Joe', 'Mary', 'Mo'
);

// basic syntax
// for ($i=0; $i < count($people); $i++) {
// only do count once version:
for ($i = 0, $max = count($people); $i < $max; $i++) {
    echo $people[$i] . "\n";
}
unset($i); // necessary clean-up

$people_2 = array(
    'John' => array('age' => 30),
    'Joe' => array('age' => 30),
    'Mary' => array('age' => 30),
    'Mo' => array('age' => 30)
);

// on a regular array, $name is the index, $details is the value
foreach ($people as $name => $details) {
    var_dump($name, $details);
    echo "\n";
}
unset($name, $details);

// on an associative array, $name is the key, $details is the value
foreach ($people_2 as $name => $details) {
    var_dump($name, $details);
    echo "\n";
}
unset($name, $details); // necessary clean-up

$str = "Māori people";

assert(strlen($str) == 13);
assert(mb_strlen($str) == 12);
assert(grapheme_strlen($str) == 12);
assert(iconv_strlen($str) == 12);

var_dump(strlen($str));
var_dump(mb_strlen($str));
var_dump(grapheme_strlen($str));
var_dump(iconv_strlen($str));

Misc::printSep();

// don't use print_r, use var_dump
print_r(true); // "1"
print_r(false); // ""
print_r(null); // ""

Misc::printSep();

var_dump(true);
var_dump(false);
var_dump(null);
echo "</code></pre>";

try {
    1/0;
} catch (\Throwable $th) {
    echo "recovered from error\n";

    var_dump($th);
    //throw $th;
}

// dump the fully qualified class name constant
var_dump(Car::class);
// phpinfo();
