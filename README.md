## Pure functions :
 - functions with no side effects
 - functions referentially transparent (for the same input always give the same output)

## Side Effects
  
 - modifying variables / DB
 - throwing an exception
 - printing to console / drawing on the screen
 
Instead of making some side effects:
```scala
def buyCoffee(cc: CreditCard): Coffee = { /* charging CreditCard */ }
```

In FP, functions return the product and side effect:
```scala
def buyCoffee(cc: CreditCard): (Coffee, Charge) = {/* no side effects */}
```
 
In FP, we push the side effects as far as possible to outer layers of the app. 
We should implement apps with a pure core and a thin layer on the outside that handles effects.
 
## Referential Transparency (RT)
 
This is another property of pure functions.

For the same input they always give the same output so they can be replaced with result without changing program.
 
1 + 1 can be replaced with 2. That's why function .+() can be called referentially transparent.
 
eg of not RT:
```scala
 val sb = new StringBuilder("Hello")
 sb.append(", World")   // Hello, World
 sb.append(", World")   // Hello, World, World
```

function which may *throw exception* - breaks RT because it doesn't always return what we expect

## Why use Pure Functions ?
 
With PF we don't have to care about :
 - multiple points of access to a function (eg: by different references)
 - in which order function will be called (side effects) or in which moment (RT).
 - state changes that may occur before/after function's call (RT)
 
We only need to focus on function's body, which make life easier.
 
This makes programs more MODULAR (modules are independent and reusable: COMPOSABLE)
 
Pure Functions are modular and composable because they separate the logic of computation, from :
 - how to obtain the input
 - what to do with result
 
Pure Functions are like black box.
   



### References:
 - https://github.com/fpinscala/fpinscala
 