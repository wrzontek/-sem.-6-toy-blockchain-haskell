-- am418419
module HashTree where

import Hashable32
import Utils (isJust)

data Tree a = Empty | Leaf Hash a | Branch Hash (Tree a) (Tree a)

addSpaces :: Integer -> String
addSpaces 0 = ""
addSpaces n = "  " ++ addSpaces (n - 1)

drawTree :: Show a => Tree a -> String
drawTree t = drawIndented t 0
   where
      drawIndented Empty _ = ""
      drawIndented (Leaf h a) n = addSpaces n ++ showHash h ++ " " ++ show a ++ "\n"
      drawIndented (Branch a b Empty) n = addSpaces n ++ showHash a ++ " +" ++ "\n" ++ drawIndented b (n + 1)
      drawIndented (Branch a b c) n = addSpaces n ++ showHash a ++ " -" ++ "\n" ++ drawIndented b (n + 1) ++ drawIndented c (n + 1)

treeHash :: Tree a -> Hash
treeHash (Leaf h a) = h
treeHash (Branch h l r) = h
treeHash Empty = 0

leaf :: Hashable a => a -> Tree a
leaf x = Leaf (hash x) x

twig :: Hashable a => Tree a -> Tree a
twig Empty = Empty
twig t = Branch (hash (tHash, tHash)) t Empty where tHash = treeHash t

node :: Hashable a => Tree a -> Tree a -> Tree a
node Empty Empty = Empty
node l Empty = Branch (hash (tHash, tHash)) l Empty where tHash = treeHash l
node Empty r = Branch (hash (tHash, tHash)) Empty r where tHash = treeHash r
node l r = Branch (hash (treeHash l, treeHash r)) l r

buildTree :: Hashable a => [a] -> Tree a
buildTree [] = Empty
buildTree [x] = leaf x
buildTree p = head $ buildNextTreeLevel (map leaf p) []
   where 
      buildNextTreeLevel :: Hashable a => [Tree a] -> [Tree a] -> [Tree a]
      buildNextTreeLevel [] [x] = [x]
      buildNextTreeLevel [] level = buildNextTreeLevel (reverse level) []
      buildNextTreeLevel (x:y:t) level = buildNextTreeLevel t (node x y : level)
      buildNextTreeLevel [x] level = buildNextTreeLevel [] (twig x : level)

type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

instance Show a => Show (MerkleProof a) where
   showsPrec p (MerkleProof x proof) = showParen (p > 7) (showString  ("MerkleProof "  ++ showParen (p < 7) (shows x) "" ++ " " ++ showMerklePath proof))

showMerklePath :: MerklePath -> String
showMerklePath [] = ""
showMerklePath ((Left l):t) = "<" ++ showHash l ++ showMerklePath t
showMerklePath ((Right r):t) = ">" ++ showHash r ++ showMerklePath t

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof x t = go x t []
   where
      go :: Hashable a => a -> Tree a -> MerklePath -> Maybe (MerkleProof a)
      go _ Empty _ = Nothing
      go x (Leaf h _) p = if hash x == h then Just $ MerkleProof x p else Nothing
      go x (Branch h l r) p
         | isJust leftProof = Just $ MerkleProof x (Left rightHash : getPathFromMaybeProof leftProof)
         | isJust rightProof = Just $ MerkleProof x (Right leftHash : getPathFromMaybeProof rightProof)
         | otherwise = Nothing
         where
            leftProof = go x l p
            rightProof = go x r p
            leftHash = if isEmpty l then hash (x, x) else treeHash l
            rightHash = if isEmpty r then hash (x, x) else treeHash r

            getPathFromMaybeProof :: Maybe (MerkleProof a) -> MerklePath
            getPathFromMaybeProof (Just (MerkleProof x path)) = path
            getPathFromMaybeProof Nothing = undefined -- nie wywołujemy tego nigdy, sprawdzane jest przed wywołaniem

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths x t = go x t []
   where
      go :: Hashable a => a -> Tree a -> MerklePath -> [MerklePath]
      go _ Empty _ = []
      go x (Leaf h _) p = [p | hash x == h]
      go x (Branch h l r) p =  map (p ++) leftPaths ++ map (p ++) rightPaths
         where
            leftPaths = go x l [Left $ treeHash r]
            rightPaths = go x r [Right $ treeHash l]

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h m = h == getProofRootHash m
   where
      getProofRootHash :: Hashable a => MerkleProof a -> Hash
      getProofRootHash (MerkleProof x []) = hash x
      getProofRootHash (MerkleProof x [Left r]) = hash (hash x, r)
      getProofRootHash (MerkleProof x [Right l]) = hash (l, hash x)
      getProofRootHash (MerkleProof x p@((Left r):t)) =  hash (getProofRootHash (MerkleProof x t), r)
      getProofRootHash (MerkleProof x p@((Right l):t)) =  hash (l, getProofRootHash (MerkleProof x t))

