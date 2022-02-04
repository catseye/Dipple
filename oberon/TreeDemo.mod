(*
  Using Vostok Oberon-07 implementation ( https://github.com/Vostok-space/vostok )
  Run with:
  ./result/ost run 'TreeDemo.Go' -infr . -m source -m ~/path/to/Dipple/oberon/
*)

MODULE TreeDemo;

IMPORT Out, BinaryTree;

PROCEDURE Go*;
  VAR n: BinaryTree.NodePtr;
BEGIN
  n := (
    BinaryTree.MakeTree(5,
      BinaryTree.MakeTree(3,
        NIL,
        BinaryTree.MakeTree(4, NIL, NIL)
      ),
      BinaryTree.MakeTree(7,
        BinaryTree.MakeTree(6, NIL, NIL),
        BinaryTree.MakeTree(12, NIL, NIL)
      )
    )
  );
  (* Note that n is opaque to us; we can't see inside it.  So we can't say, for example: *)
  (* Out.Int(n^.val, 3); *)
  BinaryTree.InOrderTraverse(n);
  Out.Ln();
END Go;

BEGIN
END TreeDemo.
