-- Using Zipper, reference: https://learnyoua.haskell.sg/content/zh-cn/ch14/zippers.html

import Control.Monad

data Tree = Leaf | Node Int Tree Tree deriving (Show, Read)
data Direction = L | R deriving (Show, Read)
data History = H Direction Int Tree deriving (Show, Read)
data Zipper = Z Tree [History] deriving (Show, Read)

bLeft :: Zipper -> Zipper
bLeft (Z (Node x l r) hs) = Z l (H L x r : hs)

bRight :: Zipper -> Zipper
bRight (Z (Node x l r) hs) = Z r (H R x l : hs)

bParent :: Zipper -> Zipper
bParent (Z t (H L x r : hs)) = Z (Node x t r) hs
bParent (Z t (H R x l : hs)) = Z (Node x l t) hs

bPrint :: Zipper -> Int
bPrint (Z (Node x _ _) _) = x

bChange :: Zipper -> Int -> Zipper
bChange (Z (Node x l r) hs) n = Z (Node n l r) hs

bInsertRight :: Zipper -> Int -> Zipper
bInsertRight (Z (Node x l r) hs) y = Z (Node x l (Node y Leaf r)) hs

bInsertLeft :: Zipper -> Int -> Zipper
bInsertLeft (Z (Node x l r) hs) y = Z (Node x (Node y Leaf l) r) hs

bAsNewRight :: Zipper -> Int -> Zipper
bAsNewRight (Z t (H d x r : hs)) y = Z t (H R y Leaf : H d x r : hs)

bIsLeftChild :: Zipper -> Bool
bIsLeftChild (Z _ (H L _ _ : _)) = True
bIsLeftChild (Z _ (H R _ _ : _)) = False

bRightN :: Zipper -> Int -> Zipper
bRightN z 1 = z
bRightN z n = bRightN (bRight z) (n - 1)

bDelSelfLeftS :: Zipper -> Zipper
bDelSelfLeftS (Z (Node _ _ rt) (H R x l : hs)) = getZipper $ visitParent (M (Z rt (H R x l : hs)))
bDelSelfLeftS (Z (Node _ _ rt) (H L x r : hs)) = Z (Node x rt r) hs

newtype Mtree = M Zipper deriving (Show, Read)

getZipper :: Mtree -> Zipper
getZipper (M z) = z

change :: Mtree -> Int -> Mtree
change (M z) x = M (bChange z x)

getValue :: Mtree -> Int
getValue (M z) = bPrint z

visitLeft :: Mtree -> Mtree
visitLeft (M z) = M (bParent z)

visitRight :: Mtree -> Mtree
visitRight (M z) = M (bRight z)

visitParent :: Mtree -> Mtree
visitParent (M z) = if bIsLeftChild z then M (bParent z) else visitParent (visitLeft (M z))

visitChild :: Mtree -> Int -> Mtree
visitChild (M z) n = M (bRightN (bLeft z) n)

insertLeft :: Mtree -> Int -> Mtree
insertLeft (M z) x = M (bAsNewRight z x)

insertRight :: Mtree -> Int -> Mtree
insertRight (M z) x = M (bInsertRight z x)

insertChild :: Mtree -> Int -> Mtree
insertChild (M z) x = M (bInsertLeft z x)

delete :: Mtree -> Mtree
delete (M z) = M (bDelSelfLeftS z)

operate :: [String] -> [Int]
operate [] = []
operate ops = operateAux ops (M (Z (Node 0 Leaf Leaf) [])) where
    operateAux [] _ = []
    operateAux (op : ops) m
        | head (words op) == "change" = operateAux ops (change m (read (words op !! 1) :: Int))
        | op == "print" = getValue m : operateAux ops m
        | head (words op) == "visit" = case words op !! 1 of
            "left" -> operateAux ops (visitLeft m)
            "right" -> operateAux ops (visitRight m)
            "parent" -> operateAux ops (visitParent m)
            "child" -> operateAux ops (visitChild m (read (words op !! 2) :: Int))
        | head (words op) == "insert" = case words op !! 1 of
            "left" -> operateAux ops (insertLeft m (read (words op !! 2) :: Int))
            "right" -> operateAux ops (insertRight m (read (words op !! 2) :: Int))
            "child" -> operateAux ops (insertChild m (read (words op !! 2) :: Int))
        | op == "delete" = operateAux ops (delete m)

main :: IO ()
main = do
    num <- (read <$> getLine) :: IO Int
    ops <- replicateM num getLine
    mapM_ print (operate ops)