<?php
declare(strict_types=1);
namespace Eoin\Util;

class Dog implements Animal
{

    public function makeNoise(): string
    {
        return "woof";
    }
}
