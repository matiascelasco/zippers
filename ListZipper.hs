module ListZipper where 

import Command (CommandParser)
import Data.List (intercalate)
import Highlight (Highlightable, get, put, highlightCurrent)
import Text.Read (readMaybe)

type Crumb t = t
type Context t = [Crumb t]
data Zipper t = Zipper (Context t) [t]

--------------------------------------------------------------------------------

instance Functor Zipper where
  fmap f (Zipper cs xs) = Zipper (fmap f cs) (fmap f xs)

--------------------------------------------------------------------------------

toList :: Zipper t -> [t]
toList (Zipper cs xs) = toList' cs xs
  where
  toList' [] xs' = xs'
  toList' (x:cs') xs' = toList' cs' (x:xs')

empty :: Zipper t
empty = Zipper [] []

new :: [t] -> Zipper t -> Zipper t
new xs _ = Zipper [] xs

instance Highlightable Zipper where
  get (Zipper _ []) = error "Can't get the current value: The list is empty"
  get (Zipper _ (x:_)) = x

  put _ (Zipper _ []) = 
    error "Can't replace the current value: the list is empty"
  put x (Zipper cs (_:xs)) = Zipper cs (x:xs)

insert :: t -> Zipper t -> Zipper t
insert x (Zipper cs []) = Zipper cs [x]
insert _ _ = error ("'insert' only can be used for empty lists. " ++ 
                    "Use 'insert before' or 'insert after' instead.")

insertBefore :: t -> Zipper t -> Zipper t
insertBefore x (Zipper cs []) = 
  error "'insert before' can't be used for empty lists. Use 'insert' instead."
insertBefore x (Zipper cs xs) = Zipper (x:cs) xs

insertAfter :: t -> Zipper t -> Zipper t
insertAfter x (Zipper cs []) = 
  error "'insert after' can't be used for empty lists. Use 'insert' instead."
insertAfter y (Zipper cs (x:xs)) = Zipper cs (x:y:xs)

next :: Zipper t -> Zipper t
next (Zipper _ [x]) = 
  error "Can't move to the next value: we are at the end of the list"
next (Zipper cs (x:xs)) = Zipper (x:cs) xs

prev :: Zipper t -> Zipper t
prev (Zipper [] _) = 
  error "Can't move to the previous value: we are at the beginning of the list"
prev (Zipper (x:cs) xs) = Zipper cs (x:xs)

delete :: Zipper t -> Zipper t
delete (Zipper (x:cs) [_]) = Zipper cs [x]
delete (Zipper cs (x:xs)) = Zipper cs xs

--------------------------------------------------------------------------------

instance (Show t) => Show (Zipper t) where  
  show z@(Zipper cs xs) = unlines [
      "Breadcrumbs: " ++ show cs,
      "Current list: " ++ show xs,
      "Current value: " ++ case xs of [] -> ""; _ -> show (get z),
      "Whole list:",
      intercalate " " $ concat $ case xs of 
        [] -> [map show (reverse cs), ["{}"]]
        _ -> [map show $ toList $ highlightCurrent z]
    ]

--------------------------------------------------------------------------------

commandParser :: (Read t) => CommandParser (Zipper t)
commandParser x = case words x of
  ("new":args) -> fmap new (traverse readMaybe args)
  ["put", v] -> fmap put (readMaybe v)
  ["insert", "before", v] -> fmap insertBefore (readMaybe v)
  ["insert", "after", v] -> fmap insertAfter (readMaybe v)
  ["next"] -> Just next
  ["prev"] -> Just prev
  ["delete"] -> Just delete
  _ -> Nothing