module Heap (
  Heap,
  empty,
  singleton,
  singletonWith,
  null,
  size,
  insert,
  insertWith,
  showMin,
  popMin,
  fromList,
  fromListWith,
  toList,
  heapSort,
  heapSortWith,
  isMember,
  delete,
  replace,
  replaceWith
  ) where

import Test.QuickCheck
import Data.List (sort, sortBy)
import Data.Function (on)
import qualified Data.IntMap as M

import qualified Data.List as L (delete)
import Prelude hiding (null)

data Tree a = Nil | Node a (Tree a) (Tree a)
data Heap a = Empty | Heap (a -> a -> Bool) Int (Tree a)

instance Show a => Show (Heap a) where
  showsPrec _ Empty = showString "fromList []"
  showsPrec d h = showParen (d > 10) $
    showString "fromList " .  showsPrec 11 (toList h)

instance Eq (Heap a) where
  Empty == Empty = True
  Empty == Heap{} = False
  Heap{} == Empty = False
  a@(Heap leq s1 _) == b@(Heap _ s2 _) = s1 == s2 && go leq (toList a) (toList b)
    where
      go f (x:xs) (y:ys) = f x y && f y x && go f xs ys
      go _ [] [] = True
      go _ _ _ = False

empty :: Heap a
empty = Empty

singleton :: (Ord a) => a -> Heap a
singleton = singletonWith (<=)

singletonWith :: (a -> a -> Bool) -> a -> Heap a
singletonWith leq x = Heap leq 1 (Node x Nil Nil)

size :: Heap a -> Int
size Empty = 0
size (Heap _ s _) = s

changeSize :: (Int -> Int) -> Heap a -> Heap a
changeSize f (Heap leq s t) = Heap leq (f s) t

null :: Heap a -> Bool
null Empty = True
null _ = False

-- Zippers

data Zip a = L {val::a, tree::(Tree a)} | R {val::a, tree::(Tree a)}

type Zipper a = (Tree a, [Zip a])

zipTree :: Tree a -> Zipper a
zipTree t = (t, [])

unzipTree :: Zipper a -> Tree a
unzipTree (t, []) = t
unzipTree z = unzipTree . zipUp $ z

zipRepVal :: a -> Zip a -> Zip a
zipRepVal v (L _ t) = L v t
zipRepVal v (R _ t) = R v t

zipLeft :: Zipper a -> Zipper a
zipLeft (Nil, z) = error "Heap.zipLeft: empty tree"
zipLeft (Node x l r, z) = (l , L x r :z)

zipRight :: Zipper a -> Zipper a
zipRight (Nil, z) = error "Heap.zipRight: empty tree"
zipRight (Node x l r, z) = (r , R x l :z)

zipUp :: Zipper a -> Zipper a
zipUp (_, []) = error "Heap.zipUp: root"
zipUp (t, L x r :zs) = (Node x t r, zs)
zipUp (t, R x l :zs) = (Node x l t, zs)

-- Moving through trees

findDir :: Int -> [Int]
findDir = tail . reverse . bin
  where bin 0 = []
        bin s = let (q,r) = quotRem s 2 in r:bin q

findMemberDir :: (Eq a) => (a -> a -> Bool) -> a -> Tree a -> [Int]
findMemberDir leq e t = reverse $ find leq e t [] where
  find _ _ Nil _ = []
  find leq e (Node x l r) d
    | e==x      = d
--    | leq e x   = []
    | zl/=[]    = zl
    | zr/=[]    = zr
    | otherwise = []
    where zl = find leq e l (0:d)
          zr = find leq e r (1:d)

bubbleUp :: (a -> a -> Bool) -> Zipper a -> Zipper a
bubbleUp leq (t, []) = (t, [])
bubbleUp leq p@(n@(Node x l r), z:zs)
  | leq (val z) x = p
  | otherwise     = bubbleUp leq $ zipUp (Node (val z) l r, zipRepVal x z:zs)

bubbleDown :: (a -> a -> Bool) -> Zipper a -> Zipper a
bubbleDown _ zpr@(Node x Nil Nil, z) = zpr
bubbleDown leq (n@(Node x (Node lx ll lr) Nil), z)
  | leq x lx  = (n, z)
  | otherwise = (Node lx (Node x ll lr) Nil, z)
bubbleDown leq (n@(Node x nl@(Node lx ll lr) nr@(Node rx rl rr)), z)
  | leq x lx && leq x rx = (n, z)
  | leq lx rx            = bubbleDown leq (Node x ll lr, L lx nr:z)
  | otherwise            = bubbleDown leq (Node x rl rr, R rx nl:z)

popLast :: Heap a -> (a, Heap a)
popLast (Heap leq s t) = popLast' (findDir s) (zipTree t)
  where popLast' [] (Node x _ _, z) = (x, Heap leq (s-1) $ unzipTree (Nil, z))
        popLast' (d:ds) z
          | d==0      = popLast' ds $ zipLeft z
          | otherwise = popLast' ds $ zipRight z

-- Operations

insert :: (Ord a) => a -> Heap a -> Heap a
insert = insertWith (<=)

insertWith :: (a -> a -> Bool) -> a -> Heap a -> Heap a
insertWith leq x Empty = singletonWith leq x
insertWith _ x (Heap leq s t) = Heap leq (s+1) t'
  where t' = unzipTree $ bubbleUp leq $ insertLast x (findDir (s+1)) (zipTree t)
        insertLast x [] (Nil, z) = (Node x Nil Nil, z)
        insertLast x (d:ds) z
          | d==0      = insertLast x ds (zipLeft z)
          | otherwise = insertLast x ds (zipRight z)

showMin :: Heap a -> a
showMin Empty =  error "Heap.findMin: empty heap"
showMin (Heap _ _ (Node x _ _))= x

popMin :: Heap a -> (a, Heap a)
popMin Empty =  error "Heap.popMin: empty heap"
popMin (Heap leq _ (Node x Nil Nil)) = (x, Empty)
popMin g = (x, Heap leq s t')
  where t' = unzipTree $ bubbleDown leq $ zipTree (Node y l r)
        (y , Heap leq s (Node x l r)) = popLast g

fromList ::  (Ord a) => [a] -> Heap a
fromList = fromListWith (<=)

fromListWith :: (a -> a -> Bool) -> [a] -> Heap a
fromListWith leq = foldr (insertWith leq) Empty

toList :: Heap a -> [a]
toList (Heap _ _ t) = toListTree t
  where toListTree Nil = []
        toListTree (Node x l r) = [x] ++ toListTree l ++ toListTree r

heapSort :: (Ord a) => [a] -> [a]
heapSort = heapSortWith (<=)

heapSortWith :: (a -> a -> Bool) -> [a] -> [a]
heapSortWith leq x = pop $ fromListWith leq x
  where pop Empty = []
        pop h = let (m, h') = popMin h in m : pop h'

isMember :: (Eq a) => a -> Heap a -> Bool
isMember e Empty = False
isMember e (Heap _ _ t) = isIn e t
  where isIn e Nil = False
        isIn e (Node x l r)
          | e==x      = True
--          | leq e x   = False
          | otherwise = isIn e l || isIn e r

replace :: (Eq a) => a -> a -> Heap a -> Heap a
replace _ _ Empty = Empty
replace old new h@(Heap leq s t) = replaceWith leq old new h

replaceWith :: (Eq a) => (a -> a -> Bool) -> a -> a -> Heap a -> Heap a
replaceWith _ _ _ Empty = Empty
replaceWith newleq old new n@(Heap leq s t)
  | isMember old n = Heap newleq s $ replace' new (findMemberDir leq old t) (zipTree t)
  | otherwise      = n
  where replace' new d zpr@(Node x l r, z)
         | d==[]     = unzipTree $ bubbleDown newleq $ bubbleUp newleq (Node new l r, z)
         | head d==0 = replace' new (tail d) (zipLeft zpr)
         | otherwise = replace' new (tail d) (zipRight zpr)

delete :: (Eq a) => a -> Heap a -> Heap a
delete e n
  | isMember e n = let (x, h) = popLast n in replace e x h
  | otherwise    = n

------------- Checks

-- Checking that heapSort works, run: quickCheck prop_sorting
prop_sorting :: (Ord a) => [a] -> Bool
prop_sorting x = sort x == heapSort x

prop_replace :: (Ord a) => [a] -> Bool
prop_replace xs = all (\x -> replace x x (fromList xs) == fromList xs) xs

prop_replaceWith ::  (Ord a) => [Int] -> [a] -> Bool
prop_replaceWith xs val = length xs > (length val ) ||
      all (\x -> replaceWith ((<=) `on` (intmap M.!)) x x h1 == h2) xs
  where intmap = M.fromList $ zip xs val
        h1 = fromListWith ((<=) `on` (intmap M.!)) xs
        h2 = fromListWith ((<=) `on` (intmap M.!)) (reverse xs)

prop_reverse :: (Ord a) => [a] -> Bool
prop_reverse xs = fromList xs == (fromList $ reverse xs)

prop_reverseWith :: (Ord a) => [Int] -> [a] -> Bool
prop_reverseWith xs val = length xs > (length val ) ||
     fromListWith ((<=) `on` (intmap M.!)) xs
       == (fromListWith ((<=) `on` (intmap M.!)) $ reverse xs )
  where intmap = M.fromList $ zip xs val

prop_fromListWith :: [Int] -> [Integer] -> Bool
prop_fromListWith nodes val = length nodes > (length val ) ||
  fromListWith ((<=) `on` (intmap M.!)) nodes
    == fromListWith ((<=) `on` (intmap M.!)) (reverse nodes)
  where intmap = M.fromList $ zip nodes val

-- Checking isMember
prop_member :: (Ord a) => [a] -> Bool
prop_member x = all (flip isMember (fromList x)) x
