<?php
declare(strict_types=1);
namespace Eoin\Util;

class Simple
{
    private string $something;

    // constructor is called __construct (is optional)
    // if you give constructor parameters a visibility keyword they are automatically defined as a property on the class
    public function __construct(public string $name = 'Simon')
    {
        // $this->name = $name; // not required because of 'public' in the parameter declaration
        $this->something = "secrety";
    }

    public function greet()
    {
        // $this refers to current instance
        echo "* {$this->name}\n";
    }
}
