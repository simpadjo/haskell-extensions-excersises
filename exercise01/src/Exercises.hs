{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Exercises where
import Data.Maybe





{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where count :: a -> Int
instance Countable Int  where count   = id
instance Countable [a]  where count   = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  CnNil :: CountableList
  CnCons :: Countable a => a -> CountableList -> CountableList


-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList CnNil = 0
countList (CnCons x xs) = (count x) + (countList xs)


-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero  CnNil = CnNil
dropZero (CnCons x xs) =
  case count x of
    0 -> dropZero xs
    _ -> CnCons x $ dropZero xs


-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

filterInts :: CountableList -> CountableList
filterInts = error "Contemplate me!"





{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  ANil :: AnyList
  ACons :: a -> AnyList -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList ANil = ANil
reverseAnyList (ACons x xs) = append x (reverseAnyList xs) where
                                  append :: a -> AnyList -> AnyList
                                  append c ANil = ACons c ANil
                                  append c (ACons y ys) = ACons y (append c ys)

filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList = undefined

countAnyList :: AnyList -> Int
countAnyList = undefined

foldAnyList :: Monoid m => AnyList -> m
foldAnyList = undefined

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList ANil = True
isEmptyAnyList (ACons _ _) = False

instance Show AnyList where
  show = error "What about me?"





{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

instance Eq a => Eq (TransformableTo a) where
  (==) (TransformWith f x) (TransformWith g y) = (f x) == (g y)


-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?
instance Functor TransformableTo where
  fmap f (TransformWith g x) = TransformWith (f . g) x





{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?
isEqual :: EqPair -> Bool
isEqual (EqPair a b) = a == b

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)
data EqPair' a where
  EqPair' :: Eq a => a -> a -> EqPair' a

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?

--No, can't constrain a w/o GADT



{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ (IntBox i _)) = i

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers EmptyBox = 1
countLayers (IntBox _ b) = 1 +  (countLayers b)
countLayers (StringBox _ b) = 1 +  (countLayers b)
countLayers (BoolBox _ b) = 1 +  (countLayers b)

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?





{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

hhead :: (HList (a, b)) -> a
hhead (HCons x xs) = x

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

--patternMatchMe :: HList (Int, String, Bool, ()) -> Int
--patternMatchMe = undefined
patternMatchMe :: HList (Int, (String, (Bool, ()))) -> Int
patternMatchMe (HCons i (HCons s (HCons b HNil))) =  i + (length s)  + (if b then 1 else 0)

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?





{- SEVEN -}

-- | Here are two data types that may help:

--data Empty = Empty
--  deriving Eq
--data Branch left centre right = Branch left centre right
--  deriving Eq

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  HEmpty :: HTree ()
  HBranch :: (HTree l) -> c -> (HTree r) -> HTree (l, c, r)

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?
deleteLeft :: HTree (l c r) -> HTree ((), c, r)
deleteLeft (HBranch  a b c) = HBranch HEmpty b c

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. Recursion is your friend here - you
-- shouldn't need to add a constraint to the GADT!

instance Eq (HTree ()) where
  (==) _ _ = True

instance (Eq (HTree a), Eq b, Eq (HTree c)) => Eq (HTree (a, b, c)) where
  (==) (HBranch x y z) (HBranch p q r) =  ((x == p) && (y == q))  && (z == r)


hTreeEqExample :: Bool
hTreeEqExample =
  let t1 = HBranch HEmpty "x" (HBranch HEmpty 1 HEmpty) in
  let t2 = HBranch HEmpty "x" (HBranch HEmpty 2 HEmpty) in
  t1 == t2


{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  AltNil :: AlternatingList a b
  AltCons :: a -> AlternatingList b a -> AlternatingList a b

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts AltNil = []
getFirsts (AltCons x xs) = x : (getSeconds xs)

getSeconds :: AlternatingList a b -> [b]
getSeconds AltNil = []
getSeconds (AltCons x xs) = getFirsts xs

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues al = (mconcat (getFirsts al), mconcat (getSeconds al))

foldValues' :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues' AltNil = (mempty, mempty)
foldValues' (AltCons x xs) = ((mappend x x1), y1) where
                                 (y1, x1) = foldValues' xs



{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval (Equals e1 e2) = (eval e1) == (eval e2)
eval (Add e1 e2) =  (eval e1) + (eval e2)
eval (If c b1 b2) = if (eval c) then (eval b1) else (eval b2)
eval (IntValue i) = i
eval (BoolValue b) = b

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

parse :: DirtyExpr -> Maybe (Expr Int)
parse (DirtyEquals _ _) = Nothing
parse (DirtyAdd e1 e2) = Add <$> (parse e1) <*> (parse e2)
parse (DirtyIf c b1 b2) = If <$> (parseB c) <*> (parse b1) <*> (parse b2)
parse (DirtyIntValue i) = Just $ IntValue i
parse (DirtyBoolValue _) = Nothing

parseB :: DirtyExpr -> Maybe (Expr Bool)
parseB (DirtyEquals _ _) = Nothing
parseB (DirtyAdd _ _) = Nothing
parseB (DirtyIf c b1 b2) = If <$> (parseB c) <*> (parseB b1) <*> (parseB b2)
parseB (DirtyIntValue _) = Nothing
parseB (DirtyBoolValue b) = Just $ BoolValue b

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe'?





{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  TNil :: TypeAlignedList a a
  TCons :: (x -> y) -> (TypeAlignedList y z) -> TypeAlignedList x z
  -- ...

-- | b. Which types are existential?

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs TNil l = l
composeTALs (TCons f xs) l  =  composeTALs xs (talAppend l f)


talAppend :: (TypeAlignedList a b) -> (b->c) -> TypeAlignedList a c
talAppend TNil f = TCons f TNil
talAppend (TCons x xs) f = TCons x (talAppend xs f)
