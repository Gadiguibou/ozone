# The Ozone (O<sub>3</sub>) programming language

Ozone is a statically-typed programming language with a focus on safety, understandability, and performance (in that order).

Ozone aims to allow anyone to write safe, simple, high-level abstractions over low-level constructs without sacrificing performance.

## Features

- Static typing
- Local type inference
- Algebraic data types and pattern matching
- Aliasing XOR mutability, fearless concurrency, and safe resource management thanks to:
  - Linear types
  - Immutable references invalidated by consuming operations
  - Explicit copying
- Generics
- Traits

## Examples

Note that the following examples are not yet valid Ozone code. They are intended to show the future syntax and semantics of the language.

### Hello, world!

The simplest version of "Hello, world!" in Ozone is:

```ozone
let main = () -> {
    println("Hello, world!");
}
```

or, without syntax sugar:

```ozone
# Functions without an explicit return type have a return type of `()`, the unit type.
let main = () -> () {
    # Expressions terminated with a semicolon must have a return value of `()`.
    let () = println("Hello, world!") in

    # The return value of the last expression in a block is returned.
    # If it is omitted, the return value is `()`.
    ()
}
```

### Recursion

The following is a simple program computing the 10th term of the Fibonacci sequence:

```ozone
let main = () -> {
    println(fib(10));
}

# Recursive bindings must use the `rec` keyword.
# Otherwise, the recursive calls to `fib` would refer to a previous binding.
let rec fib = (n: Int) -> Int {
    if n <= 1 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}
```

### Generics

Ozone supports generics in function and type definitions.

```ozone
# A generic type definition.
type Option[T] = { None() | Some(T) };

# A generic function definition.
let map[T, U] = (f: (T) -> U, option: Option(T)) -> Option(U) {
    match option {
        None() -> None(),
        Some(x) -> Some(f(x)),
    }
};
```

### Algebraic data types and pattern matching

There are two different composite types in Ozone:

- Records
- Unions

Types can be defined using the `type` keyword.

#### Records

Records are heterogeneous collections of named fields.

```ozone
# A record type.
type Point = {
    x: Int,
    y: Int,
}

# Initializing a record value.
let p: Point = Point { x: 1, y: 2 }

# A record type does not need to be defined explicitly.
let p: { x: Int, y: Int } = { x: 1, y: 2 }

# Accessing a field of a record.
let x = p.x

# Destructuring assignment on a struct
let { x: x_1, y: y_1 } = p
# Omitting field names when they match the variable names
let { x, y } = p
# Omitting unused fields
let { x, .. } = p
```

#### Tuples

Tuples are heterogeneous collections of unnamed fields.

```ozone
# A tuple type.
type Point = (Int, Int)

# Initializing a tuple value.
let p: Point = Point(1, 2)

# A tuple type does not need to be defined explicitly.
let p: (Int, Int) = (1, 2)

# Accessing a field of a tuple.
let x = p.0

# Destructuring assignment on a tuple
let (x, y) = p
# Omitting unused fields
let (x, _) = p
```

#### Unions

Unions are a collection of named variants with optional inner data.
You may know them as "tagged unions", "sum types", or "enums" in Rust.

```ozone
# A (recursive) union type.
# Union types must always be defined explicitly.
type rec Natural = {
    Zero() | Successor(Natural)
}

# Initializing a union value.
let two: Natural = Successor(Successor(Zero))

# Pattern matching on a union value.
let rec val = (n: Natural) -> Int {
    match n {
        Zero() => 0,
        Successor(n) => 1 + val(n),
    }
}
```

### Consuming operations

```ozone
let main = () -> {
    let x: Int = 5 in

    # To consume a value, use the postfix operator `!`.
    let y: Int = x! in

    # The following line would not compile, because `x` has been consumed.
    # print(x); # Error: `x` has been consumed.
};
```

### References

```ozone
let main = () -> {
    let x: Int = 5 in

    # This creates an immutable reference to `x`.
    let y: Int from x = x in

    # This creates another immutable reference to `x`.
    let z: Int from x = y in

    # References are treated like values with the following exceptions:
    # - Their type is `T from parent_name` where `T` is the type of their value and `parent_name` is the name of the variable they refer to.
    # - They cannot be consumed.
    # - They can no longer be used after the variable they are referencing is consumed.

    # The following line would not compile because `y` is a reference.
    # let w: Int = y! in # Error: `y` is a reference and references cannot be consumed.

    # This consumes `x` invalidating all references to it.
    let w = x! in
    # The following line would not compile because `x` has been consumed.
    # This invalidates `x` and all references to `x`.
    # print(y); # Error: `y` is unbound.

    print(w);
}
```

### Linear types

```ozone
let main = () -> {
    # Some types are linear, meaning that their values **must** be consumed **exactly once**.
    # This is enforced by the compiler.

    # `list_of_numbers` has type `!List[Int]` which is a linear type.
    # All linear types are prefixed with `!`.
    let list_of_numbers: !List[Int] = List::new(1, 2, 3) in

    # In general, all types which manage a resource, including memory, are linear.
    # This ensures that the resource they manage is freed.

    # Ozone does not support mutation, any mutation must be done through a consuming operation.
    # For example, to "mutate" a `!List` you must use a consuming operation like the following.
    let list_of_numbers = list_of_numbers!.push(4) in

    # References to a linear type or its contents can still be created.
    let z: Int from list_of_numbers = list_of_numbers.get(0).unwrap() in

    # The standard way to consume a linear type when you no longer need it is to call the `drop` method on it.
    list_of_numbers!.drop();
    # If the line above was not present, the program would not compile because `list_of_numbers` is never consumed.
};
```

### Safe concurrency

Linear types facilitate safe concurrency as well.

For example, a (not yet implemented) API for spawning a thread could look like this:

```ozone
let spawn[T] = (f: (!T) -> ()) -> !Thread(T) { ... }
```

The `!T` type of the argument to `f` guarantees that the thread has exclusive access to its value.
