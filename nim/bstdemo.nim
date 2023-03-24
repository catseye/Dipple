# Rudiments of a BST using inheritance in Nim
# TODO: write a version using Object Variants instead, to compare

type Tree = ref object of RootObj
  value: int

type Branch = ref object of Tree
  left: Tree
  right: Tree

type Leaf = ref object of Tree

method minValue(self: Tree): int {.base.} =
  return self.value
method minValue(self: Branch): int =
  if self.left == nil:
    return self.value
  else:
    return minValue(self.left)
method minValue(self: Leaf): int =
  return self.value

var z: Tree

z = Branch(value: 4, left: Leaf(value: 2), right: Leaf(value: 9))
echo minValue(z)

z = Branch(value: 4, left: nil, right: nil)
echo minValue(z)
