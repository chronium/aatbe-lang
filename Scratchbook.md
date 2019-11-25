# Scratchbook for ideas

### Option
Generic `[T]` sum type of Some[T] + None

Function ideas:

```
// when
val v = "true" when true // returns Some("true")
val v = "true" when false // returns None

// when def
fn when[T] (val: T, condition: bool)
   -> Option[T] = condition ? Some(val) : None

// get
val v = get Some(42) // return &42
val v = get None // panic with "Cannot get value from None"

// get def
fn get[T] (val: T) for T: Option[T]
   -> &T = &_ if val is Some(_) else panic "Cannot get value from None"
```
