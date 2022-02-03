(*
  Using Vostok Oberon-07 implementation ( https://github.com/Vostok-space/vostok )
  Run with:
  ./result/ost run 'Fibonacci.FibThirty' -infr . -m source -m ~/path/to/Dipple/oberon/
*)

MODULE Fibonacci;

IMPORT Out;

PROCEDURE Fib(count: INTEGER);
  VAR a, b, c, i: INTEGER;
BEGIN
  a := 1;
  b := 1;
  Out.Int(a, 0); Out.Ln;
  Out.Int(b, 0); Out.Ln;
  FOR i := 1 TO count DO
    c := a + b;
    Out.Int(c, 0); Out.Ln;
    a := b;
    b := c;
  END;
END Fib;

PROCEDURE FibThirty*;
BEGIN
  Fib(30);
END FibThirty;

BEGIN
END Fibonacci.
