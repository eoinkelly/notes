
type aliases vs interfaces

* both declarations define a shape

* an interface can `extend` an other interface, class, or type alias
* type aliases can by "exteneded" by intersecting them with another type e.g.

Type aliases and Interfaces are very similar:

```typescript
type Car = {
  wheels: number
}

type Fast = {
  maxSpeed: number
}

type Ferarri = Car & Fast & {
  color: string
}

type Lambo = Car & Fast

let aa: Ferarri = {
    wheels: 4,
    maxSpeed: 300,
    color: 'red'
}

let bb: Lambo = {
    wheels: 4,
    maxSpeed: 300
}

console.log(aa, bb)
```

vs

```typescript
interface Car {
  wheels: number
}

interface Fast {
  maxSpeed: number
}

interface Ferarri extends Car, Fast {
  color: string
}

interface Lambo extends Car, Fast {
}

let aa: Ferarri = {
    wheels: 4,
    maxSpeed: 300,
    color: 'red'
}

let bb: Lambo = {
    wheels: 4,
    maxSpeed: 300
}

console.log(aa, bb)
```

Differences:

1. RHS
    * Interface RHS: must be a shape
    * Type alias RHS: can be any type or expression which yields a type (e.g. A & B)
2. Interfaces get an assignability check when extending, type expressions do not
    ```typescript
    interface Car {
    wheels: number
    }

    // compiler error:
    // Interface 'Ferarri' incorrectly extends interface 'Car'.
    //   Types of property 'wheels' are incompatible.
    //   Type 'string' is not assignable to type 'number'.(2430)
    interface Ferarri extends Car {
    wheels: string
    }

    type Car2 = {
    wheels: number
    }

    // works fine
    type Ferarri2 = Car2 & {
    wheels: string
    }
    ```
3. Automatic merging
    * Mutliple interfaces with the same name in the same scope are automatically merged
    * Multiple type aliases with same name in same scope is a compile error



