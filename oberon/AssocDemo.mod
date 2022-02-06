MODULE AssocDemo;

IMPORT Out, AssocList;

PROCEDURE DisplayValue*(v: AssocList.ValuePtr);
BEGIN
  IF v # NIL THEN
    Out.String(v^.str);
  ELSE
    Out.String("...");
  END;
END DisplayValue;

PROCEDURE Scan(list: AssocList.ListPtr);
  VAR
    k: INTEGER;
BEGIN
  FOR k := 1 TO 10 DO
    Out.Int(k * 10, 3);
    Out.String(" ");
    DisplayValue(AssocList.Lookup(list, k * 10));
    Out.Ln;
  END;
  Out.Ln
END Scan;

PROCEDURE Demo*;
  VAR
    a, b: AssocList.ListPtr;
    r: BOOLEAN;
BEGIN
  a := AssocList.Empty();
  r := AssocList.Insert(a, 40, "Hello");
  ASSERT(r);
  r := AssocList.Insert(a, 80, "World");
  ASSERT(r);
  r := AssocList.Insert(a, 20, "!");
  ASSERT(r);
  Scan(a);
  AssocList.Remove(a, 80);
  Scan(a);

  b := AssocList.Empty();
  r := AssocList.Insert(b, 30, "Wonderful");
  ASSERT(r);
  r := AssocList.Insert(b, 70, "Fantastic");
  ASSERT(r);

  r := AssocList.Merge(a, b);
  ASSERT(r);
  Scan(a)

END Demo;

BEGIN
END AssocDemo.
