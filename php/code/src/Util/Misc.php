<?php
declare(strict_types=1);
namespace Eoin\Util;

class Misc
{
    public static function sum(int $a, int $b): int
    {
        return $a + $b;
    }

    public static function printSep(): void
    {
        echo "\n===========\n";
    }
}
