'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.create = exports.drive = void 0;
const drive = car => {
  console.log(`Drivin' car with ${car.numWheels} wheels`);
  return true;
};
exports.drive = drive;
const create = () => {
  return {
    make: 'Something',
    model: 'other',
    numWheels: 4,
    numCylinders: 8
  };
};
exports.create = create;
