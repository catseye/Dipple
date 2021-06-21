module StringJazz

open FStar.String

let s = "These"

let _ = assert (s = "These")

let _ = assert ((lowercase "These") = "these")

// let _ = assert ((lowercase s) = "these")     // Nope.  "assertion failed".  I am unsure why.
