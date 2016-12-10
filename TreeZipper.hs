module TreeZipper where

import Command (CommandParser)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Tree as T
import Data.Tree.Pretty (drawVerticalTree)
import Highlight (Highlightable, get, put, highlightCurrent)
import qualified ListZipper as LZ
import Text.Read (readMaybe)

data Crumb t = Crumb (LZ.Zipper (T.Tree t))

type Context t = [Crumb t]

data Zipper t = Zipper (Context t) (T.Tree t)

--------------------------------------------------------------------------------

instance Functor Crumb where
  fmap f (Crumb lz) = Crumb (fmap (fmap f) lz)

instance Functor Zipper where
  fmap f (Zipper cs t) = Zipper (map (fmap f) cs) (fmap f t)

--------------------------------------------------------------------------------

leaf :: t -> Zipper t
leaf v = Zipper [] (T.Node v [])

new :: [t] -> Zipper t -> Zipper t
new (x:xs) _ = Zipper [] (T.Node x (map (\x' -> T.Node x' []) xs))

instance Highlightable Zipper where 
  get (Zipper _ (T.Node v _)) = v
  put v (Zipper cs (T.Node _ children)) = Zipper cs (T.Node v children)

insertChildAt :: t -> Int -> Zipper t -> Zipper t
insertChildAt v i (Zipper cs tree) 
  | 0 < i && i <= length children + 1 = Zipper cs newTree
  | otherwise = error (show i ++ " it's not a valid child position.")
  where
  (prevs, nexts) = splitAt (i - 1) children
  newTree = T.Node parentValue (prevs ++ (T.Node v []:nexts))
  T.Node parentValue children = tree

child :: Int -> Zipper t -> Zipper t
child i (Zipper crumbs (T.Node parentValue children))
  | 0 < i && i <= length children = Zipper (newCrumb:crumbs) child
  | otherwise = error (show i ++ " it's not a valid child position.")
    where
    (cs, child:xs) = splitAt (i - 1) children
    siblingsZipper = LZ.Zipper (reverse cs) ((T.Node parentValue []):xs)
    newCrumb = Crumb siblingsZipper

prevSibling :: Zipper t -> Zipper t
prevSibling (Zipper [] _) =
  error "The current node is the root, so there are no siblings"
prevSibling (Zipper [Crumb (LZ.Zipper [] _)] _) =
  error "The current node is the first child, so there are no previous siblings"
prevSibling (Zipper (Crumb crumb:cs) tree) = 
  Zipper (Crumb newCrumb:cs) prevSibling'
    where
    hole = get crumb
    prevSibling' = get (LZ.prev crumb)
    newCrumb = put hole (LZ.prev (put tree crumb))

nextSibling :: Zipper t -> Zipper t
nextSibling (Zipper [] _) =
  error "The current node is the root, so there are no siblings"
nextSibling (Zipper [Crumb (LZ.Zipper _ [])] _) =
  error "The current node is the last child, so there are no next siblings"
nextSibling (Zipper (Crumb crumb:cs) tree) = 
  Zipper (Crumb newCrumb:cs) nextSibling'
    where
    hole = get crumb
    nextSibling' = get (LZ.next crumb)
    newCrumb = put hole (LZ.next (put tree crumb))

up :: Zipper t -> Zipper t
up (Zipper [] _) =
  error "The current node is the root, so there is no parent"
up (Zipper (crumb:cs) tree) = Zipper cs parent
  where
  parent = T.Node parentValue children
  Crumb siblingsZipper = crumb
  T.Node parentValue _ = get siblingsZipper
  children = LZ.toList (put tree siblingsZipper)

delete :: Zipper t -> Zipper t
delete (Zipper [] _) = error "Can't delete the whole tree"
delete (Zipper (Crumb crumb:cs) tree) = Zipper cs parent
  where
  parent = T.Node parentValue children
  T.Node parentValue _ = get crumb
  children = LZ.toList (LZ.delete crumb)

--------------------------------------------------------------------------------

instance (Show t) => Show (Crumb t) where
  show = drawVerticalTree . treeWithHole
    where
    treeWithHole (Crumb lz) = T.Node (show v) children
      where
      T.Node v _ = get lz
      children = LZ.toList $ put hole (fmap (fmap show) lz)
      hole = T.Node "(hole)" []

instance (Show t) => Show (Zipper t) where
  show z@(Zipper cs t) = unlines $ concat [
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
    currentValue = ["Current value:", show (get z)]
    currentSubtree = case cs of 
      [] -> []
      _ -> ["Current subtree:", drawVerticalTree (fmap show t)]
    wholeTree = [
        "Whole tree:",
        drawVerticalTree $ fmap show (root (highlightCurrent z))
      ]

    root :: Zipper t -> T.Tree t
    root (Zipper [] t') = t'
    root z' = root (up z')

--------------------------------------------------------------------------------

commandParser :: (Read t) => CommandParser (Zipper t)
commandParser command = case words command of
  ["put", v] -> fmap put (readMaybe v)
  ("new":args) -> fmap new (traverse readMaybe args)
  ["insert", v, "at", "child", i] -> 
    insertChildAt <$> (readMaybe v) <*> (readMaybe i)
  ["child", i] -> fmap child (readMaybe i)
  ["next", "sibling"] -> Just nextSibling
  ["prev", "sibling"] -> Just prevSibling
  ["up"] -> Just up
  ["delete"] -> Just delete
  _ -> Nothing