type 'a int_bst =
  | Node of 'a int_bst * int * 'a * 'a int_bst
  | Leaf

