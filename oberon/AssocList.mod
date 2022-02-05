MODULE AssocList;

IMPORT Out;

TYPE
  Value* = ARRAY 256 OF CHAR;
  ListPtr* = POINTER TO List;
  List = RECORD
    key: INTEGER;
    val: Value;
    next: ListPtr
  END;

PROCEDURE Make*(key: INTEGER; val: ARRAY OF CHAR; next: ListPtr): ListPtr;
  VAR e: ListPtr;
BEGIN
  NEW(e);
  e^.key := key;
  e^.val := val;
  e^.next := next;
  RETURN e
END Make;

PROCEDURE Find*(e: ListPtr; key: INTEGER): ListPtr;
  VAR
    f: ListPtr;
    result: ListPtr;
BEGIN
  f := e;
  result := NIL;
  WHILE (f # NIL) & (result = NIL) DO
    IF f^.key = key THEN
      result := f;
    END;
    f := f^.next;
  END;
  RETURN result
END Find;

PROCEDURE Remove*(VAR e: ListPtr; key: INTEGER);
  VAR
    f: ListPtr;
    prev: ListPtr;
BEGIN
  f := e;
  prev := NIL;
  WHILE (f # NIL) DO
    IF f^.key = key THEN
      IF prev = NIL THEN
        e := f^.next;
      ELSE
        prev^.next := f^.next;
      END;
      (* DISPOSE(f); *)
      f := NIL;
    END;
    IF f # NIL THEN
      prev := f;
      f := f^.next;
    END;
  END;
END Remove;

PROCEDURE DisplayValue*(e: ListPtr);
BEGIN
  IF e # NIL THEN
    Out.String(e^.val);
  ELSE
    Out.String("...");
  END;
END DisplayValue;

PROCEDURE Scan(list: ListPtr);
  VAR
    k: INTEGER;
BEGIN
  FOR k := 1 TO 10 DO
    Out.Int(k * 10, 3);
    DisplayValue(Find(list, k * 10));
    Out.Ln;
  END
END Scan;

PROCEDURE Demo*;
  VAR
    list: ListPtr;
BEGIN
  list := Make(40, "Hello", Make(80, "World!", Make(20, "!", NIL)));
  Scan(list);
  Remove(list, 80);
  Scan(list)
END Demo;

BEGIN
END AssocList.
