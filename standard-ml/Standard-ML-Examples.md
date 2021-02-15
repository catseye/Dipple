Standard ML Examples
====================

    -> Functionality "Evaluate Standard ML expression"
    -> is implemented by shell command
    -> "cp %(test-body-file) /tmp/foo.sml && echo ';printVal it' >>/tmp/foo.sml && mosml -quietdec /tmp/foo.sml"

    -> Tests for functionality "Evaluate Standard ML expression"

Some basic expressions.

    1
    ===> 1

    ~100 * ~100 * ~100
    ===> ~1000000

A conditional.

    if true then 5 else 6
    ===> 5

The basic usage of `let` to create name bindings.

    let val a = 2 in a end
    ===> 2

    let fun r(x) = 77 in r(1) end
    ===> 77

    let fun r(x) = x in r(66) end
    ===> 66

You can also use `val` and `fn` to define function values in a `let`.

    let val r = fn(x) => x in r(66) end
    ===> 66

ML's `let` is like Scheme's `let*`: later bindings can refer to earlier ones.

    let
      fun r(x) = x * 2
      fun p(x) = r(x) * 2
    in
      p(10)
    end
    ===> 40

However in a `let`, earlier bindings cannot refer to later ones.

Skipping because I don't know why `mosml` doesn't report this error under `falderal`.
It does when you run it from the command line.

>    main() =
>    let
>      fun oddp(x)  = if x = 0 then false else evenp(x - 1)
>      fun evenp(x) = if x = 0 then true else oddp(x - 1)
>    in
>      evenp(6)
>    end
>    ???> Unbound value identifier: evenp

You need to use `rec` in order to let earlier bindings refer to later ones,
and you need to use `and` between all the mutually recursive functions.

    let
      val rec oddp = fn(x)  => if x = 0 then false else evenp(x - 1)
      and rec evenp = fn(x) => if x = 0 then true else oddp(x - 1)
    in
      evenp(6)
    end
    ===> true

    let
      val rec oddp = fn(x)  => if x = 0 then false else evenp(x - 1)
      and rec evenp = fn(x) => if x = 0 then true else oddp(x - 1)
    in
      evenp(7)
    end
    ===> false
