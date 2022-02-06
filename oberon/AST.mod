MODULE AST;

IMPORT Out;

TYPE
  ASTPtr* = POINTER TO AST;
  AST = RECORD
  END;
  ImplPtr = POINTER TO Impl;
  Impl = RECORD(AST)
    lhs: ASTPtr;
    rhs: ASTPtr
  END;
  ConjPtr = POINTER TO Conj;
  Conj = RECORD(AST)
    lhs: ASTPtr;
    rhs: ASTPtr
  END;
  DisjPtr = POINTER TO Disj;
  Disj = RECORD(AST)
    lhs: ASTPtr;
    rhs: ASTPtr
  END;
  PropVarPtr = POINTER TO PropVar;
  PropVar = RECORD(AST)
    name: CHAR
  END;

PROCEDURE MkImpl*(lhs: ASTPtr; rhs: ASTPtr): ImplPtr;
  VAR n: ImplPtr;
BEGIN
  NEW(n);
  n^.lhs := lhs;
  n^.rhs := rhs;
  RETURN n
END MkImpl;

PROCEDURE MkConj*(lhs: ASTPtr; rhs: ASTPtr): ConjPtr;
  VAR n: ConjPtr;
BEGIN
  NEW(n);
  n^.lhs := lhs;
  n^.rhs := rhs;
  RETURN n
END MkConj;

PROCEDURE MkDisj*(lhs: ASTPtr; rhs: ASTPtr): DisjPtr;
  VAR n: DisjPtr;
BEGIN
  NEW(n);
  n^.lhs := lhs;
  n^.rhs := rhs;
  RETURN n
END MkDisj;

PROCEDURE MkPropVar*(name: CHAR): PropVarPtr;
  VAR n: PropVarPtr;
BEGIN
  NEW(n);
  n^.name := name;
  RETURN n
END MkPropVar;

PROCEDURE Display*(n: ASTPtr);
BEGIN
  IF n IS PropVarPtr THEN
    Out.Char(n(PropVarPtr)^.name);
  ELSIF n IS ImplPtr THEN
    Out.String("(");
    Display(n(ImplPtr)^.lhs);
    Out.String(" -> ");
    Display(n(ImplPtr)^.rhs);
    Out.String(")");
  ELSIF n IS ConjPtr THEN
    Out.String("(");
    Display(n(ConjPtr)^.lhs);
    Out.String(" & ");
    Display(n(ConjPtr)^.rhs);
    Out.String(")");
  ELSIF n IS DisjPtr THEN
    Out.String("(");
    Display(n(DisjPtr)^.lhs);
    Out.String(" | ");
    Display(n(DisjPtr)^.rhs);
    Out.String(")");
  END
END Display;

PROCEDURE Demo*;
  VAR n: ASTPtr;
BEGIN
  n := MkImpl(MkConj(MkPropVar("p"), MkPropVar("q")), MkDisj(MkPropVar("p"), MkPropVar("q")));
  Display(n);
  Out.Ln()
END Demo;

BEGIN
END AST.
