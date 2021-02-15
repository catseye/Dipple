fun show_arg () =
  case CommandLine.arguments () of
      [arg] => print ("The argument is " ^ arg ^ "\n")
    | _     => print "Usage: mosmlout arg\n\n"

fun main() =
  show_arg()
