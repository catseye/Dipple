MODULE AssocList;

IMPORT Out;

TYPE
  ValuePtr* = POINTER TO Value;
  Value* = RECORD
    str*: ARRAY 256 OF CHAR
  END;
  ListPtr* = POINTER TO List;
  List = RECORD
    key: INTEGER;
    val: ValuePtr;
    next: ListPtr
  END;

PROCEDURE Empty*(): ListPtr;
BEGIN
  RETURN NIL
END Empty;

PROCEDURE Lookup*(e: ListPtr; key: INTEGER): ValuePtr;
  VAR
    f: ListPtr;
    result: ValuePtr;
BEGIN
  f := e;
  result := NIL;
  WHILE (f # NIL) & (result = NIL) DO
    IF f^.key = key THEN
      result := f^.val;
    END;
    f := f^.next;
  END;
  RETURN result
END Lookup;

PROCEDURE Insert*(VAR e: ListPtr; key: INTEGER; strval: ARRAY OF CHAR): BOOLEAN;
  VAR
    f: ListPtr;
    v: ValuePtr;
    r: BOOLEAN;
BEGIN
  r := FALSE;
  IF Lookup(e, key) = NIL THEN
    NEW(f);
    f^.key := key;
    NEW(v);
    v^.str := strval;
    f^.val := v;
    f^.next := e;
    e := f;
    r := TRUE
  END
  RETURN r
END Insert;

PROCEDURE Remove*(VAR e: ListPtr; key: INTEGER);
  VAR
    f: ListPtr;
    prev: ListPtr;
BEGIN
  f := e;
  prev := NIL;
  WHILE f # NIL DO
    IF f^.key = key THEN
      IF prev = NIL THEN
        e := f^.next;
      ELSE
        prev^.next := f^.next;
      END;
      (* The Oberon-07 report does not come out and say it, but
         there is no way to explicitly deallocate memory that
         has been allocated with NEW().  One presumably relies
         on garbage collection to occur, instead of saying: *)
      (* DISPOSE(f); *)
      f := NIL;
    END;
    IF f # NIL THEN
      prev := f;
      f := f^.next;
    END;
  END;
END Remove;

(*
 * Given two assoc lists A and B, merge them.  For all (key, value) pairs
 * in B, if the key is not in A, insert (key, value) to A.  If the key is
 * in A, then if the value matches what it is A, do nothing.  If the value
 * does not match, signal an error.  Not intended to be efficient.
 *)
PROCEDURE Merge*(VAR a: ListPtr; b: ListPtr): BOOLEAN;
  VAR
    f: ListPtr;
    found: ValuePtr;
    inserted: BOOLEAN;
    successful: BOOLEAN;
BEGIN
  successful := TRUE;
  f := b;
  WHILE successful & (f # NIL) DO
    found := Lookup(a, f^.key);
    IF found = NIL THEN
      (* Should always succeed *)
      inserted := Insert(a, f^.key, f^.val^.str);
    ELSIF found^.str # f^.val^.str THEN
      successful := FALSE
    END;
    f := f^.next
  END
  RETURN successful
END Merge;

BEGIN
END AssocList.
