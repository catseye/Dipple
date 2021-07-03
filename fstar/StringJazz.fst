module StringJazz

open FStar.String

let s = "These"

let _ = assert (s = "These")

let _ = assert ((lowercase "These") = "these")

// You might think this should work, but it seems
// `assert` is not powerful enough for this job:
//
// let _ = assert ((lowercase s) = "these")

// One can ask F* to reduce the expression
// `lowercase s` to its simplest form instead.
// This is powerful enough to do the job:

let proof_of_lowercase ()
  : Lemma ((lowercase s) = "these")
  = normalize_term_spec (lowercase s)
