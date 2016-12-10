module BinaryTreeZipper where 

import Command (CommandParser)
import qualified Data.Tree as T
import Data.Tree.Pretty (drawVerticalTree)
import qualified Data.Vector as V
import Highlight (Highlightable, get, put, highlightCurrent)
import Text.Read (readMaybe)

data BinaryTree t = Empty 
                  | Node (BinaryTree t) t (BinaryTree t)

data Crumb t = LeftHoleBT t (BinaryTree t) 
             | RightHoleBT (BinaryTree t) t

type Context t = [Crumb t]

data Zipper t = Zipper (Context t) (BinaryTree t)

--------------------------------------------------------------------------------

instance Functor BinaryTree where  
  fmap _ Empty = Empty
  fmap f (Node l v r) = (Node (fmap f l) (f v) (fmap f r))

instance Functor Crumb where
  fmap f (LeftHoleBT v r) = LeftHoleBT (f v) (fmap f r)
  fmap f (RightHoleBT l v) = RightHoleBT (fmap f l) (f v)

instance Functor Zipper where
  fmap f (Zipper cs bt) = Zipper (map (fmap f) cs) (fmap f bt)

--------------------------------------------------------------------------------

empty :: Zipper t
empty = Zipper [] Empty

new :: [t] -> Zipper t -> Zipper t
new xs _ = Zipper [] (new' 1)
  where 
  v = V.fromList xs
  new' i | i > V.length v = Empty
            | otherwise = Node l x r
            where
              l = new' (i * 2)
              x = v V.! (i - 1)
              r = new' (i * 2 + 1)

instance Highlightable Zipper where 
  get (Zipper _ Empty) = error "The tree is empty"
  get (Zipper _ (Node _ v _)) = v

  put _ (Zipper _ Empty) = 
    error "Can't replace the current value: the tree is empty"
  put v (Zipper cs (Node l _ r)) = Zipper cs (Node l v r)

insertLeft :: t -> Zipper t -> Zipper t
insertLeft lv (Zipper cs (Node Empty v r)) =
  Zipper cs (Node (Node Empty lv Empty) v r)
insertLeft _ _ = error $ concat [
    "'insert left' is only allowed when the tree is not ", 
    "empty and the current node has no left child. "
  ]

insertRight :: t -> Zipper t -> Zipper t
insertRight rv (Zipper cs (Node l v Empty)) =
  Zipper cs (Node l v (Node Empty rv Empty))
insertRight _ _ = error $ concat [
    "'insert right' is only allowed when the tree is not ", 
    "empty and the current node has no right child. "
  ]

left :: Zipper t -> Zipper t
left (Zipper _ Empty) = error "Can't move left: the tree is empty"
left (Zipper _ (Node Empty _ _)) = 
  error "Can't move left: there's no left child"
left (Zipper cs (Node l v r)) = Zipper (crumb:cs) l
  where
  crumb = LeftHoleBT v r

right :: Zipper t -> Zipper t
right (Zipper _ Empty) = error "Can't move right: the tree is empty"
right (Zipper _ (Node _ _ Empty)) = 
  error "Can't move right: there's no right child"
right (Zipper cs (Node l v r)) = Zipper (crumb:cs) r
  where
  crumb = RightHoleBT l v

sibling :: Zipper t -> Zipper t
sibling (Zipper [] _) = 
  error "The current tree has no sibling"
sibling (Zipper ((LeftHoleBT _ Empty):_) _) = 
  error "The current tree has no sibling"
sibling (Zipper ((RightHoleBT Empty _):_) _) =
  error "The current tree has no sibling"
sibling (Zipper ((LeftHoleBT v r):cs) l) = Zipper ((RightHoleBT l v):cs) r
sibling (Zipper ((RightHoleBT l v):cs) r) = Zipper ((LeftHoleBT v r):cs) l

up :: Zipper t -> Zipper t
up (Zipper [] _) = 
  error "Can't move up: we are at the top"
up (Zipper (LeftHoleBT v r:cs) l) = Zipper cs (Node l v r)
up (Zipper (RightHoleBT l v:cs) r) = Zipper cs (Node l v r)

delete :: Zipper t -> Zipper t
delete (Zipper [] _) = empty
delete (Zipper cs _) = up (Zipper cs Empty)

--------------------------------------------------------------------------------

prepareToShowBinaryTree :: (Show t) => BinaryTree t -> T.Tree String
prepareToShowBinaryTree Empty = T.Node "empty" []
prepareToShowBinaryTree (Node l v r) = 
  T.Node (show v) (map prepareToShowBinaryTree [l, r])

prepareToShowCrumb :: (Show t) => Crumb t -> T.Tree String
prepareToShowCrumb crumb = case crumb of 
  (LeftHoleBT v r)  -> T.Node (show v) [hole, prepareToShowBinaryTree r]
  (RightHoleBT l v) -> T.Node (show v) [prepareToShowBinaryTree l, hole]
  where
  hole = T.Node "(hole)" []

instance (Show t) => Show (BinaryTree t) where
  show = drawVerticalTree . prepareToShowBinaryTree

instance (Show t) => Show (Crumb t) where
  show = drawVerticalTree . prepareToShowCrumb

instance (Show t) => Show (Zipper t) where
  show z@(Zipper cs bt) = unlines $ concat [
      level,
      breadcrumbs,
      currentValue,
      currentSubtree,
      wholeTree
    ]
    where
    level = case (length cs) + 1 of
      1 -> ["We are at the root of the tree"]
      x -> ["We are at level " ++ show x ++ " of the tree"]
    breadcrumbs = "Breadcrumbs:":(case cs of 
        [] -> ["[]"]
        _ -> map show (reverse cs)
      )
    currentValue = case bt of 
      Empty -> []
      _ -> ["Current value:", show (get z)]
    currentSubtree = case cs of 
      [] -> []
      _ -> ["Current subtree:", show bt]
    wholeTree = ["Whole tree:", case bt of
        Empty -> "empty"
        _ -> show $ root $ highlightCurrent z
      ]

    root :: Zipper t -> BinaryTree t
    root (Zipper [] bt') = bt'
    root z' = root (up z')

--------------------------------------------------------------------------------

commandParser :: (Read t) => CommandParser (Zipper t)
commandParser x = case words x of
  ["put", v] -> fmap put (readMaybe v)
  ("new":args) -> fmap new (traverse readMaybe args)
  ["insert", "left", v] -> fmap insertLeft (readMaybe v)
  ["insert", "right", v] -> fmap insertRight (readMaybe v)
  ["left"] -> Just left
  ["right"] -> Just right
  ["sibling"] -> Just sibling
  ["up"] -> Just up
  ["delete"] -> Just delete
  _ -> Nothing