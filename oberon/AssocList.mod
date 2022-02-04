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

PROCEDURE DisplayValue*(e: ListPtr);
BEGIN
  IF e # NIL THEN
    Out.String(e^.val);
  ELSE
    Out.String("(NIL!)");
  END;
  Out.Ln;
END DisplayValue;

PROCEDURE Demo*;
  VAR
    e: ListPtr;
    v: ListPtr;
    k: INTEGER;
BEGIN
  e := Make(40, "Hello", Make(80, "World!", NIL));
  FOR k := 1 TO 10 DO
    v := Find(e, k * 10);
    DisplayValue(v);
  END
END Demo;

BEGIN
END AssocList.
