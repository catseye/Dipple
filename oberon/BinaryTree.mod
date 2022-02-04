MODULE BinaryTree;

IMPORT Out;

TYPE
  NodePtr* = POINTER TO Node;
  Node = RECORD
    val: INTEGER;
    left: NodePtr;
    right: NodePtr
  END;

PROCEDURE MakeTree*(val: INTEGER; left: NodePtr; right: NodePtr): NodePtr;
  VAR n: NodePtr;
BEGIN
  NEW(n);
  n^.val := val;
  n^.left := left;
  n^.right := right;
  RETURN n
END MakeTree;

PROCEDURE InOrderTraverse*(n: NodePtr);
BEGIN
  IF n # NIL THEN
    InOrderTraverse(n^.left);
    Out.Int(n.val, 3);
    InOrderTraverse(n^.right);
  END;
END InOrderTraverse;

BEGIN
END BinaryTree.
