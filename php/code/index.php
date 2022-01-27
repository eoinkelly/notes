<?php

namespace Eoin;

/**
 * Class Car
 * @author Eoin
 */
class Car
{
  function drive()
  {
    echo "Driving!";
    return null;
  }

}

$car = new Car();
$car->drive();