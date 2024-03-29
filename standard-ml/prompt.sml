fun prompt() =
  (TextIO.output(TextIO.stdOut, "prompt> ");
   TextIO.flushOut(TextIO.stdOut);
   TextIO.input(TextIO.stdIn))

fun strip(s) = substring(s, 0, size(s) - 1)

fun execute(k) =
  TextIO.output(TextIO.stdOut, "(do " ^ strip(k) ^ ")\n")

fun loop() =
  case prompt() of
      "quit\n" => true
    | k        => (execute(k); loop())

fun hello () =
  (print "Hello? ";
  let val k = TextIO.input(TextIO.stdIn) in
    print ("You typed " ^ strip(k) ^ "!\n")
  end)

fun main() =
  (hello(); loop())

val _ = main ();
