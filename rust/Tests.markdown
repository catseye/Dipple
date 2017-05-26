Tested Rust Snippets
====================

This file is a Falderal suite of snippets of code written in Rust.

The goal is to provide a document which contains a lot of small Rust
examples, both readable by a human browsing Github, and auto-testable
by a computer.  (This is a fib.  The real goal is for me to exercise
Falderal while I learn Rust.)

This document reads kind of like a tutorial, and kind of like a specification,
while in actuality it is kind of neither.  You may like this style, or you may
not; I think it's a matter of personal taste.

This document is also sort of opinionated.  If I have feelings about Rust,
I will not hesitate to include them here.  It is also not intended to be
exhaustive; things like floating-point types and bitwise operators don't
interest me all that much, and they can easily be looked up in the reference
manual when needed.

The version of Rust being used is the current `master` version as of this
writing (which we all hope will become stable and acquire a version number
RSN of course.)

To run the tests, have Falderal installed (i.e. have the `falderal` program
on your search path; I recommend `shelf` for this) and run
`falderal --substring-error Tests.markdown`.

    -> Tests for functionality "Compile and run Rust program"

    -> Functionality "Compile and run Rust program"
    -> is implemented by shell command
    -> "rustc %(test-file) -o a.out && ./a.out"

Some Rudiments
--------------

`println!` prints a string on standard output.

    | fn main() {
    |   println!("Hello, world!");
    | }
    = Hello, world!

`println!` supports string formatting (`std::fmt`).

    | fn main() {
    |   println!("Hello, {:s}!", "world");
    | }
    = Hello, world!

Here's how you format an integer into a string.

    | fn main() {
    |   println!("Catch-{:d}", 22);
    | }
    = Catch-22

And actually, `std::fmt` can infer the needed types.

    | fn main() {
    |   println!("{}-{}", "Catch", 22);
    | }
    = Catch-22

Variables
---------

`let` binds a local variable.

    | fn main() {
    |   let message = "Hello, world!";
    |   println!("{:s}", message);
    | }
    = Hello, world!

By default, local variables are immutable, but you can re-bind them.

    | fn main() {
    |   let message = "Hello, world!";
    |   println!("{:s}", message);
    |   let message = "Goodbye, world!";
    |   println!("{:s}", message);
    | }
    = Hello, world!
    = Goodbye, world!

But you can't actually change the value of an immutable variable:

    | fn main() {
    |   let section = 22;
    |   println!("Catch-{:d}", section);
    |   section = section * 2;
    |   println!("Catch-{:d}", section);
    | }
    ? re-assignment of immutable variable `section`

If you want to be able to change the value of a variable, declare it as
mutable.

    | fn main() {
    |   let mut section = 22;
    |   println!("Catch-{:d}", section);
    |   section = section * 2;
    |   println!("Catch-{:d}", section);
    | }
    = Catch-22
    = Catch-44

Although Rust can (usually?) infer the type of a variable, you can also
explicitly note the type in the `let` (and possibly other places?)

    | fn main() {
    |   let section: int = 22;
    |   let name: &str = "Catch";
    |   println!("{:s}-{:d}", name, section);
    | }
    = Catch-22

Somewhat oddly, you can declare an immutable variable without any value,
then bind it later on.

    | fn main() {
    |   let district;
    |   print!("Welcome to ");
    |   district = 9;
    |   println!("District {:d}", district);
    | }
    = Welcome to District 9

You can't use it until you bind it, of course.

    | fn main() {
    |   let district;
    |   println!("District {:d}", district);
    |   district = 9;
    |   println!("District {:d}", district);
    | }
    ? use of possibly uninitialized variable: `district`

Control Structures
------------------

Unsurprisingly, Rust has an `if` statement.  It doesn't have a dedicated
`elsif` or `elif` or anything, it does the good ol' dangling-else thing.

    | fn main() {
    |   if 3 > 4 {
    |     println!("ORLY?");
    |   } else if 5 < 4 {
    |     println!("Pull the other one!");
    |   } else {
    |     println!("I see what you did there.");
    |   }
    | }
    = I see what you did there.

`if` can be used as an expression.

    | fn main() {
    |   println!("{:s}", 
    |     if 3 > 4 {
    |       "ORLY?"
    |     } else if 5 < 4 {
    |       "Pull the other one!"
    |     } else {
    |       "I see what you did there."
    |     }
    |   );
    | }
    = I see what you did there.

Also unsurprisingly, Rust has a `while` statement.

    | fn main() {
    |   let mut count = 0;
    |   while count < 5 {
    |     print!("{:d} ", count);
    |     count += 1;
    |   }
    |   println!("done");
    | }
    = 0 1 2 3 4 done

You can break out of a `while` with `break`.

    | fn main() {
    |   let mut count = 0;
    |   while count < 100 {
    |     print!("{:d} ", count);
    |     count += 1;
    |     if (count > 4) {
    |       break;
    |     }
    |   }
    |   println!("done");
    | }
    = 0 1 2 3 4 done

And you can `continue`.

    | fn main() {
    |   let mut count = 0;
    |   while count < 10 {
    |     count += 1;
    |     if count % 2 == 0 {
    |       continue;
    |     }
    |     print!("{:d} ", count);
    |   }
    |   println!("done");
    | }
    = 1 3 5 7 9 done

There is also a `match` statement, which handles multiple cases.  Note
the commas.

    | fn main() {
    |   let mut count = 0;
    |   while count <= 8 {
    |     print!("{:s}", match count {
    |       0      => "Z",
    |       1 | 5  => "/",
    |       2 .. 4 => "@",
    |       _      => "."
    |     });
    |     count += 1;
    |   }
    |   println!("");
    | }
    = Z/@@@/...

If you don't like commas, you can use blocks.

    | fn main() {
    |   let mut count = 0;
    |   while count <= 8 {
    |     print!("{:s}", match count {
    |       0      => { "Z" }
    |       1 | 5  => { "/" }
    |       2 .. 4 => { "@" }
    |       _      => { "." }
    |     });
    |     count += 1;
    |   }
    |   println!("");
    | }
    = Z/@@@/...

Cases in a `match` must be exhaustive.  Which is awesome and proper and is
one of the things that makes me want to get to know Rust better, because
this is a good design decision.

    | fn main() {
    |   let mut count = 0;
    |   while count <= 8 {
    |     print!("{:s}", match count {
    |       0      => { "Z" }
    |       1 | 5  => { "/" }
    |       2 .. 4 => { "@" }
    |     });
    |     count += 1;
    |   }
    |   println!("");
    | }
    ? non-exhaustive patterns

Blocks
------

The value of a block is the value of the last expression in the block.

    | fn main() {
    |   let result = {
    |     let mut count = 0;
    |     while count < 5 {
    |       count += 1
    |     }
    |     count
    |   };
    |   println!("{:d}", result);
    | }
    = 5

Unless there's a semicolon.  (The error message is saying, basically, that
it we are telling Rust to do an arithmetic operation (format with `{:d}`)
on a value of "void" or "unit" type, and it (very reasonably) doesn't know
how to do that.)

    | fn main() {
    |   let result = {
    |     let mut count = 0;
    |     while count < 5 {
    |       count += 1
    |     }
    |     count;
    |   };
    |   println!("{:d}", result);
    | }
    ? failed to find an implementation of trait std::fmt::Signed for ()

Note also that an assignment is not an expression.

    | fn main() {
    |   let result = {
    |     let mut count = 0;
    |     count = 8
    |   };
    |   println!("{:d}", result);
    | }
    ? failed to find an implementation of trait std::fmt::Signed for ()

And that `while` doesn't evaluate to anything, itself.

    | fn main() {
    |   let mut count = 0;
    |   let result =
    |     while count < 5 {
    |       count += 1;
    |       count
    |     };
    | }
    ? mismatched types: expected `()` but found `<generic integer #0>`

You usually need braces, even if there is only one expression in a block.

    | fn main()
    |   println!("{:s}", 
    |     if 3 > 4
    |       "ORLY?"
    |     else if 5 < 4
    |       "Pull the other one!"
    |     else
    |       "I see what you did there.");
    ? expected `{` but found `println`

Empty blocks are syntactically possible.

    | fn main() {
    |   if 3 > 4 {} else { println!("Good.") }
    | }
    = Good.

Functions
---------

Rust lets you write functions??  That's a relief!

    | fn bleep() {
    |   println!("Bleep!");
    | }
    | fn main() {
    |   bleep();
    |   bleep();
    | }
    = Bleep!
    = Bleep!

Functions can take arguments???  My cup runneth over!

    | fn bleep(x: int) {
    |   println!("Bleep {:d}!", x);
    | }
    | fn main() {
    |   bleep(7);
    |   bleep(49);
    | }
    = Bleep 7!
    = Bleep 49!

Functions can return values.  You don't need a `return`, but do remember to
omit the semicolon.

    | fn square(x: int) -> int {
    |   x * x
    | }
    | fn main() {
    |   println!("Vat {:d}", square(7));
    | }
    = Vat 49

But if you want to use `return`, you can.

    | fn square(x: int) -> int {
    |   return x * x;
    |   return x * x * x;
    | }
    | fn main() {
    |   println!("Vat {:d}", square(7));
    | }
    = Vat 49

The types of arguments, and the return type, must be given.

    | fn square(x) {
    |   x * x
    | }
    | fn main() {
    |   println!("Vat {:d}", square(7));
    | }
    ? expected `:` but found `)`

Functions can recurse.  Now we're cooking with gas!

    | fn factorial(x: int) -> int {
    |   if x <= 1 { 1 } else { x * factorial(x - 1) }
    | }
    | fn print_factorial(x: int) {
    |   println!("{:d}! = {:d}", x, factorial(x));
    | }
    | fn main() {
    |   print_factorial(6);
    | }
    = 6! = 720

Functions can be mutually recursive.  No need to forward-declare them.

    | fn odd(x: int) -> bool {
    |   if x == 1 { true } else if x == 0 { false } else { even(x - 1) }
    | }
    | fn even(x: int) -> bool {
    |   if x == 2 { true } else if x == 1 { false } else { odd(x - 1) }
    | }
    | fn main() {
    |   println!("{:b} {:b} {:b} {:b}",
    |     odd(7), odd(8), even(7), even(8)
    |   );
    | }
    = true false false true

Functions can be passed to functions.

    | fn double(x: int) -> int {
    |   x * 2
    | }
    | fn square(x: int) -> int {
    |   x * x
    | }
    | fn apply_a_bunch(f: fn(int) -> int) {
    |   print!("{:d} ", f(7));
    |   print!("{:d} ", f(9));
    |   println!("{:d}", f(0));
    | }
    | fn main() {
    |   apply_a_bunch(double);
    |   apply_a_bunch(square);
    | }
    = 14 18 0
    = 49 81 0

Functions can be returned from functions.

    | fn double(x: int) -> int {
    |   x * 2
    | }
    | fn square(x: int) -> int {
    |   x * x
    | }
    | fn give_me_a_function(r: bool) -> fn(int) -> int {
    |   if r { square } else { double }
    | }
    | fn main() {
    |   println!("{:d}", give_me_a_function(false)(7));
    |   println!("{:d}", give_me_a_function(true)(7));
    | }
    = 14
    = 49

Functions can be defined local to a function.

    | fn give_me_a_function(r: bool) -> fn(int) -> int {
    |   fn double(x: int) -> int {
    |     x * 2
    |   }
    |   fn square(x: int) -> int {
    |     x * x
    |   }
    |   if r { square } else { double }
    | }
    | fn main() {
    |   println!("{:d}", give_me_a_function(false)(7));
    |   println!("{:d}", give_me_a_function(true)(7));
    | }
    = 14
    = 49

Functions can be anonymous.  The syntax differs remarkably, though, and
that makes me suspect there's something else going on here.  (Although,
as far as I can tell, nothing else *has* to be going on here; this is all
still just function values — I haven't gotten to closures yet.)

    | fn give_me_a_function(r: bool) -> |int| -> int {
    |   if r {
    |     | x: int | -> int { x * x }
    |   } else {
    |     | x: int | -> int { x * 2 }
    |   }
    | }
    | fn main() {
    |   println!("{:d}", give_me_a_function(false)(7));
    |   println!("{:d}", give_me_a_function(true)(7));
    | }
    = 14
    = 49

Lack of Closures So Far
-----------------------

Functions that are defined local to a function are *not* closures.

    | fn give_me_a_function(r: bool) -> fn(int) -> int {
    |   fn double(x: int) -> int {
    |     println!("{:b}", r);
    |     x * 2
    |   }
    |   fn square(x: int) -> int {
    |     println!("{:b}", r);
    |     x * x
    |   }
    |   if r { square } else { double }
    | }
    | fn main() {
    |   println!("{:d}", give_me_a_function(false)(7));
    |   println!("{:d}", give_me_a_function(true)(7));
    | }
    ? can't capture dynamic environment in a fn item; use the || { ... } closure form instead

Anonymous functions, as I've used them above, *are* closures.  But now,
we are getting into Rust Pointer Madness\[tm\], so.  Ahem.  Yes.  (The full
error message is quite interesting, as the compiler tries to explain why
the requirements are conflicting.)

    | fn give_me_a_function(r: bool) -> |int| -> int {
    |   if r {
    |     | x: int | -> int {
    |       println!("{:b}", r);
    |       x * x 
    |     }
    |   } else {
    |     | x: int | -> int {
    |       println!("{:b}", r);
    |       x * 2
    |     }
    |   }
    | }
    | fn main() {
    |   println!("{:d}", give_me_a_function(false)(7));
    |   println!("{:d}", give_me_a_function(true)(7));
    | }
    ? cannot infer an appropriate lifetime due to conflicting requirements

So let's backtrack a bit over the parts that have been missed, so we can
come back to closures prepared...

Integer Types
-------------

We've seen `str`, `int`, and `bool`.  There are also a lot of fiddly little
variations on `int`, of course.

    | fn main() {
    |   let a : int = 7;
    |   let b : uint = 7;
    |   let c : i8 = 7;
    |   let d : u8 = 7;
    |   let e : i16 = 7;
    |   let f : u16 = 7;
    |   let g : i32 = 7;
    |   let h : u32 = 7;
    |   let i : i64 = 7;
    |   let j : u64 = 7;
    |   println!("{} {} {} {} {} {} {} {} {} {}",
    |            a, b, c, d, e, f, g, h, i, j);
    | }
    = 7 7 7 7 7 7 7 7 7 7

Let's see what happens if we try to assign two integer variables of different
signedness to each other.

    | fn main() {
    |   let mut a : int = 7;
    |   let mut b : uint;
    |   b = a;
    |   println!("{}", b);
    | }
    ? mismatched types: expected `uint` but found `int`

So how do we coerce a signed int to unsigned?

    | fn main() {
    |   let mut a : int = 7;
    |   let mut b : uint;
    |   b = a as uint;
    |   println!("{}", b);
    | }
    = 7

So what happens if we coerce a value to a type that can't take it?  Answer:
the value changes.  (In just the way you'd expect if you've been programming
for a while.)

    | fn main() {
    |   let mut a : u8 = 255;
    |   let mut b : i8;
    |   b = a as i8;
    |   println!("{}", b);
    | }
    = -1

Integer types have signedness, but all types have a size.  That is, values of a
type occupy a certain number of bytes of memory, determined solely by their type.
`std::mem::size_of` gets a type's size.

    | fn main() {
    |   println!("{} {}",
    |            std::mem::size_of::<u32>(), std::mem::size_of::<i64>());
    | }
    = 4 8

Let's see what happens if we try to assign two integer variables of different
size to each other.  First, let's try assigning a value that won't fit.

    | fn main() {
    |   let mut a : i32 = 7;
    |   let mut b : i16;
    |   b = a;
    |   println!("{}", b);
    | }
    ? mismatched types: expected `i16` but found `i32`

Ah, but if the value will fit — does it get promoted automatically?  Answer:
no, it doesn't.

    | fn main() {
    |   let mut a : i16 = 7;
    |   let mut b : i32;
    |   b = a;
    |   println!("{}", b);
    | }
    ? mismatched types: expected `i32` but found `i16`

But we can promote it explicitly, and because the destination type is bigger,
it shouldn't lose any information.

    | fn main() {
    |   let mut a : i16 = 32000;
    |   let mut b : i32;
    |   b = a as i32;
    |   println!("{}", b);
    | }
    = 32000

And what happens if we change an value to something outside its range?
Answer: the value changes to stay within the range.  (In just the way you'd
expect if you've been programming for a while.)

    | fn main() {
    |   let mut a : u8 = 255;
    |   a += 1;
    |   println!("{}", a);
    | }
    = 0

Compound Types
--------------

There are tuples.  They have no name, and their contents have no names.

    | fn main() {
    |   let x = (7, 9, 11);
    |   println!("{}", x);
    | }
    = (7, 9, 11)

Tuples support equality testing.

    | fn main() {
    |   println!("{}", (1, 2, 3) == (1, 2, 3));
    | }
    = true

Tuples are ordered lexicographically.

    | fn main() {
    |   let x = (1, 2);
    |   let y = (1, 3);
    |   let z = (2, 1);
    |   println!("{} {} {} {} {} {}",
    |    y > x, z > y, z > x, y < z, x < y, x < z);
    | }
    = true true true true true true

The size of a tuple is the sum of the sizes of the types it contains...
but padded.

    | fn main() {
    |   println!("{}", std::mem::size_of::<(u32, u8, i16)>());
    | }
    = 8

There are also named tuples, where the structure is named but the contents
aren't.  Rust calls these _tuple structs_.

    | struct I32Pair(i32, i32);
    | fn main() {
    |   let x = I32Pair(7, 9);
    |   println!("{}", std::mem::size_of::<I32Pair>());
    | }
    = 8

Unlike tuples, they aren't supported automatically by fmt.

    | struct IntPair(int, int);
    | fn main() {
    |   let x = IntPair(7, 9);
    |   println!("{}", x);
    | }
    ? failed to find an implementation of trait std::fmt::Show for IntPair

And they don't support equality testing.

    | struct IntPair(int, int);
    | fn main() {
    |   let x = IntPair(7, 9);
    |   println!("{}", x);
    | }
    ? failed to find an implementation of trait std::fmt::Show for IntPair

Nor do they support ordering.  No `deriving (Show, Ord, Eq)` for you!
(Oh, I'm sure there's *some* way to add these things, but it might not be
quite as nice as Haskell.)

    | struct IntPair(int, int);
    | fn main() {
    |   let x = IntPair(7, 9);
    |   let y = IntPair(8, 1);
    |   println!("{}", y > x);
    | }
    ? binary operation `>` cannot be applied to type `IntPair`

It seems the way to extract a value from a tuple or tuple struct is to use
a `match` statement.

    | fn main() {
    |   let x = (7, 9, 11);
    |   let z = match x {
    |     (y, _, _) => y
    |   };
    |   println!("{}", z);
    | }
    = 7

    | struct IntPair(int, int);
    | fn main() {
    |   let x = IntPair(7, 9);
    |   let z = match x {
    |     IntPair(_, y) => y
    |   };
    |   println!("{}", z);
    | }
    = 9

There are enumeration types.  These are declared and named.  Note how
Rust can tell that the `match` here is exhaustive.

    | enum State {
    |   Off,
    |   Ready,
    |   Running
    | }
    | fn main() {
    |   let s = Off;
    |   match s {
    |     Off => println!("it's off..."),
    |     Ready => println!("it's ready."),
    |     Running => println!("it's running!")
    |   }
    | }
    = it's off...

Enumeration values can have equivalents in some other type, called
"discriminator values".  Personally, I think this is gratuitous, since
you might not have or want one "canonical" set of discriminator values for
a given enum, and it's easy enough to write a function that translates one
type to the other.

    | enum State {
    |   Off = 0xdeadbeef,
    |   Ready = 1,
    |   Running = 0xffffffff
    | }
    | fn main() {
    |   println!("{}", Running as uint);
    | }
    = 4294967295

If you leave off a discriminator value, ... OK, that's even more gratuitous.

    | enum State {
    |   Off = 0,
    |   Ready = 8,
    |   Running
    | }
    | fn main() {
    |   println!("{}", Running as uint);
    | }
    = 9

Can you use strings as discriminator values?  Even though that would be very
convenient for some things, no, you can't.

    | enum State {
    |   Off = "Off",
    |   Ready = "Ready",
    |   Running = "Running"
    | }
    | fn main() {
    |   println!("{}", Running);
    | }
    ? mismatched types: expected `<generic integer #0>` but found `&'static str`

Two enumerations with enumeration values with colliding names?  Unfortunately,
the following would seem to make sense to me, but no.  You probably have to
fiddle with namespaces and `use` or something, which I will get to (much)
later.

    | enum TreePart {
    |   Bark,
    |   Trunk,
    |   Leaf
    | }
    | enum DogSound {
    |   Whine,
    |   Bark,
    |   Howl
    | }
    | fn main() {
    |   let x: DogSound = Bark;
    |   let y: TreePart = Bark;
    | }
    ? duplicate definition of value `Bark`

Enumerations can also have named tuples as their possible values.  To me,
it seems a bit weird to conflate an algebraic data type with an enum which
works like C's what with the autoincremented discriminator values and all,
but fine, OK.

    | enum IntOrNothing {
    |   YesItsAnInt(int),
    |   NoItsNotAnInt
    | }
    | fn print_intornothing(z: IntOrNothing) {
    |   match z {
    |     YesItsAnInt(d) => println!("{}", d),
    |     NoItsNotAnInt => println!("naw")
    |   }
    | }
    | fn main() {
    |   let x: IntOrNothing = YesItsAnInt(4);
    |   let y: IntOrNothing = NoItsNotAnInt;
    |   print_intornothing(x);
    |   print_intornothing(y);
    | }
    = 4
    = naw

There are structs.  They are named, and their fields have names too.

    | enum Street { Angell, Hope, Main }
    | struct Address {
    |   house_number: u64,
    |   street: Street
    | }
    | fn street_name(x : Street) -> &str {
    |   match x {
    |     Angell => "Angell",
    |     Hope => "Hope",
    |     Main => "Main"
    |   }
    | }
    | fn main() {
    |   let x = Address { house_number: 454, street: Angell };
    |   println!("{} {} St.", x.house_number, street_name(x.street));
    | }
    = 454 Angell St.

Oddly, enum and struct definitions don't need to be followed by a
semicolon, but tuple struct definitions do.

    | enum Street { Angell, Hope, Main }
    | struct IntPair(int, int)
    | struct Address {
    |   house_number: u64,
    |   street: Street
    | }
    ? expected `;` but found `struct`

Lack of Reference Types so far
------------------------------

In a hundred years' time (when we are all coding in TURKEY BOMB), if Rust will
be remembered at all, it will be remembered for its reference types.

But that's a good thing.  Because Rust is aiming to replace (or at least,
improve upon) C++.  And references in C++ are a nightmare.  If you work at
a large C++ shop, chances are there are several different reference object
libraries to choose from (with names like `auto_ptr`, `managed_ptr`,
`fast_ptr` etc. etc.) and chances are different ones are used in different
parts of the code base and chances are they are not compatible.

Rust's system of reference types is complicated, but it's a lot less
complicated than that sort of mess, and obviates the need for that sort of
mess, by providing its own built-in, prescriptive, *canonical* mess.

So.  I've already been using `&str` in a couple of places in the snippets
above, without explaining why.  In fact it's a bit cargo-cultish, as, when
I wrote them, I didn't remember exactly why, but I remembered you could use
`&`, and I did, and the tests pass.  In the next section, I will try to figure
out, and explain, why.

Ownership
---------

Central to Rust's reference types is the concept of ownership.

A function owns the values in its local variables, much like how in C/C++,
values are allocated on the stack unless you put in the extra work to allocate
them in some other way.  Of course, if a function returns a value to its caller,
the caller then owns it.  (Well, the function instance is gone anyway.)

Rust calls `&` a *reference* and describes it as a "non-owning view of a value".
If we think about bare-metal C, it just has pointers.  They are "non-owning"
in some sense, because C doesn't give a hoot about who owns what; that's
the programmer's job.  So, for the nonce, I shall think of `&` to be
something like a C pointer, and just try some things.

Note that, because I'm only using `&` in these snippets, no value ever actually
*changes* ownership... they just get "borrowed" for a while.

Here, `y` is a non-owning view of `x`.  Of course, `x` is immutable, so this
isn't much different from a copy, at least semantically.

    | fn main() {
    |   let x = "Hello";
    |   println!("{}", x);
    |   let y = &x;
    |   println!("{}", y);
    | }
    = Hello
    = Hello

Clearly, the type-inference in `str::fmt` is helping us out a bit there.
Prefix `*` appears to be the dereferencing operator.  Let's try being
explicit.

    | fn main() {
    |   let x = "Hello";
    |   println!("{}", x);
    |   let y = &x;
    |   println!("{}", *y);
    | }
    = Hello
    = Hello

Can you take a reference to a reference?  Er, apparently you can, although
I would be loathe to recommend ever doing this unless you absolutely had
to, I think.  (In C too, I've found it's often better to just make a struct
that contains a single pointer-type field, and take pointers to that struct,
than to mess with pointers-to-pointers.)

    | fn main() {
    |   let x = "Hello";
    |   println!("{}", x);
    |   let y = &(&x);
    |   println!("{}", **y);
    | }
    = Hello
    = Hello

What if you get the number of `*`s wrong?  Thankfully, it's an error.

    | fn main() {
    |   let x = "Hello";
    |   println!("{}", x);
    |   let y = &(&x);
    |   println!("{}", ***y);
    | }
    ? type `&'static str` cannot be dereferenced

OK, let's try making a non-owning view of a mutable variable, and changing
the underlying value.  Should work like changing a value in a pointed-to
variable in C, yes?  No.  Rust is cleverer than that.  It knows very well
what that sort of thing leads to, and stops you.

    | fn main() {
    |   let mut a = 100;
    |   let mut b = 200;
    |   let c = &a;
    |   println!("{}", c);
    |   a = 7;
    |   println!("{}", c);
    | }
    ? cannot assign to `a` because it is borrowed

But can we use `&` to do pass-by-reference?  First, some obviously wrong
attempts.  Here, we don't make the argument mutable *or* a reference.

    | fn inc(x: int) {
    |   x = x + 1;
    | }
    | fn main() {
    |   let mut a = 100;
    |   inc(a);
    |   println!("{}", a);
    | }
    ? cannot assign to immutable argument `x`

Here, we make the argument mutable, but not a reference.  The change is
only local to `inc`.

    | fn inc(mut x: int) {
    |   x = x + 1;
    | }
    | fn main() {
    |   let mut a = 100;
    |   inc(a);
    |   println!("{}", a);
    | }
    = 100

Here, we don't modify it, but we show that we can return a value based on it.

    | fn inc(x: &int) -> int {
    |   *x + 1
    | }
    | fn main() {
    |   let mut a = 100;
    |   println!("{}", inc(&a));
    | }
    = 101

Here, we make the *argument* mutable, but not the thing it is a reference *to*.

    | fn inc(mut x: &int) {
    |   *x = *x + 1;
    | }
    | fn main() {
    |   let mut a = 100;
    |   inc(&a);
    |   println!("{}", a);
    | }
    ? cannot assign to immutable dereference of `&`-pointer `*x`

Here, we get the function signature right, but forget to pass in a reference
to `a`.

    | fn inc(x: &mut int) {
    |   *x = *x + 1;
    | }
    | fn main() {
    |   let mut a = 100;
    |   inc(a);
    |   println!("{}", a);
    | }
    ? mismatched types: expected `&mut int` but found `<generic integer #0>`

Here, we forget to take a *mutable* reference of the mutable variable `a`.
Gosh.  References have their own mutability!

    | fn inc(x: &mut int) {
    |   *x = *x + 1;
    | }
    | fn main() {
    |   let mut a = 100;
    |   inc(&a);
    |   println!("{}", a);
    | }
    ? cannot borrow immutable dereference of `&`-pointer as mutable

Here, we get it right.

    | fn inc(x: &mut int) {
    |   *x = *x + 1;
    | }
    | fn main() {
    |   let mut a = 100;
    |   inc(&mut a);
    |   println!("{}", a);
    | }
    = 101

I'm not sure how a literal can be mutable, but apparently...

    | fn inc(x: &mut int) -> int {
    |   *x = *x + 1;
    |   return *x;
    | }
    | fn main() {
    |   println!("{}", inc(&mut 23));
    | }
    = 24

So in conclusion, to try to explain why I've had to use `&str` in the
snippets before this section; well, my explanation is still a bit fuzzy,
but I think it is this: a `str` is a value of indeterminate size, and
a local variable can only hold (and a function can only return) a value
of determinate size, like a reference.  So that is why I've had to say
`let s: &str = "foo"` and `fn x(y: int) -> &str` above.

Boxes
-----

In C you can allocate values on the heap with `malloc()` (or write your
own allocator using `sbrk()` if you're feeling peppy,) and C++ you can
allocate values on the heap by — well, let's not even go there.

In Rust, you can allocate values on the heap with `~`, But
**ownership still applies**.  The bit of code that allocated the heap
value still owns it, and you can't share it with other bits of code
without them transferring ownership to them.  It is for this reason that
Rust calls `~` an _owned box_.  (And, apparently, it does not permit
borrowing, as references do — at least not in the same way(?))

    | fn double(x: &int) -> int {
    |   return *x * 2;
    | }
    | fn main() {
    |   println!("{}", double(~23));
    | }
    = 46

Some of these snippets will just be repeats of the snippets about
references.  Where boxes differ, however, there will be some differences.

    | fn main() {
    |   let x = "Hello";
    |   println!("{}", x);
    |   let y = ~x;
    |   println!("{}", y);
    | }
    = Hello
    = Hello

Clearly, the type-inference in `str::fmt` is helping us out a bit there.
Prefix `*` appears to be the dereferencing operator.  Let's try being
explicit.

    | fn main() {
    |   let x = "Hello";
    |   println!("{}", x);
    |   let y = ~x;
    |   println!("{}", *y);
    | }
    = Hello
    = Hello

Can you make an owned box of an owned box?  Apparently you can.

    | fn main() {
    |   let x = "Hello";
    |   println!("{}", x);
    |   let y = ~(~x);
    |   println!("{}", **y);
    | }
    = Hello
    = Hello

What if you get the number of `*`s wrong?  Thankfully, it's an error.

    | fn main() {
    |   let x = "Hello";
    |   println!("{}", x);
    |   let y = ~(~x);
    |   println!("{}", ***y);
    | }
    ? type `&'static str` cannot be dereferenced

OK, let's try making an owned box of a mutable variable, and changing
the underlying value.  Should work like changing a value in a pointed-to
variable in C, yes?  No.  Making an owned box containing a variable, it
would seem, makes a copy of the value in that variable.

    | fn main() {
    |   let mut a = 100;
    |   let mut b = 200;
    |   let mut c = ~a;
    |   println!("{}", c);
    |   a = 7;
    |   println!("{}", c);
    |   *c = 80;
    |   println!("{}", c);
    |   println!("{}", a);
    | }
    = 100
    = 100
    = 80
    = 7

So let's try making a mutable owned box.

    | fn main() {
    |   let mut a: ~int = ~100;
    |   let mut b: int = 200;
    |   let mut c: ~int;
    |   c = a;
    |   println!("{}", c);
    |   *a = 7;
    |   println!("{}", c);
    | }
    ? use of moved value: `a`

Aha!  The assigment of `a` to `c` in line 5 moved the ownership of the box
from the variable `a` to the variable `c`, and prevented us changing the
contents of the box via `a` in line 7.

Can we move it back?

    | fn main() {
    |   let mut a: ~int = ~100;
    |   let mut b: int = 200;
    |   let mut c: ~int;
    |   c = a;
    |   *c = 4;
    |   println!("{}", c);
    |   a = c;
    |   *a = 7;
    |   println!("{}", a);
    | }
    = 4
    = 7

There is *no* sharing of owned boxes, so we can't even examine the
contents of `c` after we move ownership back to `a`.

    | fn main() {
    |   let mut a: ~int = ~100;
    |   let mut b: int = 200;
    |   let mut c: ~int;
    |   c = a;
    |   *c = 4;
    |   println!("{}", c);
    |   a = c;
    |   *a = 7;
    |   println!("{}", c);
    | }
    ? use of moved value: `c`

Let's do the pass-by-reference thing with an owned box.  Note that we
don't have to make a "mutable box" like we needed to make a mutable
reference.

    | fn inc(x: &mut int) {
    |   *x = *x + 1;
    | }
    | fn main() {
    |   let mut a = ~100;
    |   inc(a);
    |   println!("{}", *a);
    | }
    = 101

Note that the mistake here is creating a box and passing it to `inc`, but
not retaining the box in any way.  `a` does not change.

    | fn inc(x: &mut int) {
    |   *x = *x + 1;
    | }
    | fn main() {
    |   let mut a = 100;
    |   inc(~a);
    |   println!("{}", a);
    | }
    = 100

Can we write the pass-by-reference function to know that it takes an owned
box?  Yes, *if* we tell it the argument is mutable.

    | fn inc(mut x: ~int) {
    |   *x = *x + 1;
    | }
    | fn main() {
    |   let mut a = 100;
    |   inc(~a);
    |   println!("{}", a);
    | }
    = 100

Note that it doesn't make any sense to tell it that the argument is a
"mutable box", or to try to pass a "mutable box" to it — these are syntax
errors.

    | fn inc(x: ~mut int) {
    |   *x = *x + 1;
    | }
    | fn main() {
    |   let mut a = 100;
    |   inc(~a);
    |   println!("{}", a);
    | }
    ? found `mut` in ident position

    | fn inc(x: ~int) {
    |   *x = *x + 1;
    | }
    | fn main() {
    |   let mut a = 100;
    |   inc(~mut a);
    |   println!("{}", a);
    | }
    ? found `mut` in ident position

And what if we try it the other way around — pass a mutable reference to
a function that is declared to take a box?  Yeah, no.

    | fn inc(mut x: ~int) {
    |   *x = *x + 1;
    | }
    | fn main() {
    |   let mut a = 100;
    |   inc(&mut a);
    |   println!("{}", a);
    | }
    ? expected ~-ptr but found &-ptr

Data Structures with Reference Types
------------------------------------

Alright, so we have heap allocation.  And what is heap allocation useful for?
Allocating an amount of data which is not known statically.  Which is, er,
appropriately enough usually called dynamic allocation.

To do that, though, we have to store some kind of reference inside some kind
of data structure.  The reference may or may not refer to more data.  The
simplest data structure that fits this bill is a linked list.

(It's no surprise that this is going to look a bit like the linked list
example in the Rust tutorial because it's the "only" way to approach
dynamic allocation, based on what we've got so far.)

    | enum List {
    |   Node(int, ~List),
    |   Null
    | }
    | fn print_list(l : ~List) {
    |   match *l {
    |     Node(data, next) => {
    |       print!("{} ", data);
    |       print_list(next);
    |     }
    |     Null => {
    |       println!("done!");
    |     }
    |   }
    | }
    | fn main() {
    |   let mut a: ~List = ~Null;
    |   let mut c = 0;
    |   while c < 5 {
    |     a = ~Node(c, a);
    |     c += 1;
    |   }
    |   print_list(a);
    | }
    = 4 3 2 1 0 done!

(Aside: WOW, this reminds me a lot of the work I did on Castile.)
