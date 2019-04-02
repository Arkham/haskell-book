module BinaryTree where

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

insert' :: Ord a
        => a
        -> BinaryTree a
        -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

aTree :: BinaryTree Int
aTree =
  Node (Node Leaf 1 Leaf)
  2
  (Node Leaf 3 Leaf)

mapTree :: (a -> b)
        -> BinaryTree a
        -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree fun (Node left a right) =
  Node (mapTree fun left) (fun a) (mapTree fun right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) =
  (a : preorder left ) ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) =
  inorder left ++ (a : inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) =
  postorder left ++ postorder right ++ [a]

foldTree :: (a -> b -> b)
         -> b
         -> BinaryTree a
         -> b
foldTree _ acc Leaf = acc
foldTree fold acc (Node left a right) =
  let
    leftResult =
      fold a (foldTree fold acc left)
  in
  foldTree fold leftResult right
