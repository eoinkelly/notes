// public interface

// choosing to export multiple named things instead of one default thing

export const drive = (car: Car): boolean => {
  console.log(`Drivin' car with ${car.numWheels} wheels`);
  return true;
};

export interface Car {
  make: string;
  model: string;
  numWheels: number;
  numCylinders: number;
}

export const create = (): Car => {
  return {
    make: "Something",
    model: "other",
    numWheels: 4,
    numCylinders: 8,
  };
};

// private stuff
