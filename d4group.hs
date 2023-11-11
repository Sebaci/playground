{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module D4Group where

import Data.Complex (Complex ((:+)))
import Data.List (find)
import Data.Maybe (fromJust)

{-
first, we conceptualize directions, clockwise
enum facilitates clockwise rotation
-}

data Dir = U | R | D | L deriving (Enum, Show)

rotate :: Dir -> Dir
rotate L = U
rotate d = succ d

rotateN :: Int -> Dir -> Dir
rotateN n dir = toEnum $ (fromEnum dir + n) `mod` 4

{-
slightly different perspective:
instead of thinking about directions and applying rotations,
think about possible rotations and combining them
this is C_4 group, wich is izomorphic to Z_4 group
-}

data C4 = I | R90 | R180 | R270 deriving (Enum, Show)

instance Semigroup C4 where
  r1 <> r2 = toEnum $ (fromEnum r1 + fromEnum r2) `mod` 4

instance Monoid C4 where
  mempty = I

-- for completness, let's define the group

class Monoid a => Group a where
  inv :: a -> a

instance Group C4 where
  inv a = toEnum $ (4 - fromEnum a) `mod` 4

-- sidenote: if we want to define power operation, we can simply use mconcat
pow :: Monoid a => a -> Int -> a
pow a n = mconcat $ replicate n a

{-
another way to define C_4 is to define the isomorphic group of fourth roots of 1,
the isomorphism and then make use of it
we'll define the group via newtype and equip it with smart constructor
we use GeneralizedNewtypeDeriving to derive basic numerical operations
-}

newtype Root4thOf1 = Root4thOf1 (Complex Float) deriving (Num)

mk4thRootOf1 :: Complex Float -> Root4thOf1
mk4thRootOf1 num
  | num `elem` allowed = Root4thOf1 num
  | otherwise = error "Given number is not 4th root of 1"
  where
    allowed = [1, -1, 0 :+ 1, 0 :+ (-1)]

instance Semigroup Root4thOf1 where
  (<>) = (*)

instance Monoid Root4thOf1 where
  mempty = mk4thRootOf1 1

instance Group Root4thOf1 where
  inv (Root4thOf1 x) = mk4thRootOf1 (1 / x)

{-
we redefine C_4 to apply isomorphism this time
isomorphism is defined as pair of homomorphisms (f, g)
you could use Enum with fancy yet more obscure calculations
here it is easy to see that the isomorphism holds
-}

data C4' = I' | R90' | R180' | R270' deriving (Show)

f :: C4' -> Root4thOf1
f I' = mk4thRootOf1 1
f R90' = mk4thRootOf1 $ 0 :+ 1
f R180' = mk4thRootOf1 (-1)
f R270' = mk4thRootOf1 $ 0 :+ (-1)

g :: Root4thOf1 -> C4'
g (Root4thOf1 1) = I'
g (Root4thOf1 (0 :+ 1)) = R90'
g (Root4thOf1 (-1)) = R180'
g (Root4thOf1 (0 :+ (-1))) = R270'

-- now the instances using the isomorphism are very simple; note the order of elements

instance Semigroup C4' where
  r1 <> r2 = g (f r1 <> f r2)

instance Monoid C4' where
  mempty = I'

instance Group C4' where
  inv = g . inv . f

{-
a few examples

>>> R90' <> R90'
R180'

>>> R180' <> R180'
I'

>>> R90' <> R180' <> R90'
I'

>>> pow R90' 4
I'

>>> inv R270'
R90'

-}

{-
moving to a harder example we'll define D_4 group, which includes C_4 as a subgroup
4 rotations + 4 symmetries, which are not commutative - we can't simply use numbers
matrices representing linear transformations are a perfect tool
we'll also use Eq typeclass
-}

data D4 = Dr0 | Dr1 | Dr2 | Dr3 | Ds0 | Ds1 | Ds2 | Ds3 deriving (Eq, Show)

data Trans2dMat = Mat Int Int Int Int deriving (Eq, Show)

mk2dTransMat :: (Int, Int, Int, Int) -> Trans2dMat
mk2dTransMat (a, b, c, d)
  | (diagEmpty && b' == 1 && c' == 1) || (antidiagEmpty && a' == 1 && d' == 1) =
    Mat a b c d
  | otherwise = error "Incorrect matrix elements"
  where
    (a', b', c', d') = (abs a, abs b, abs c, abs d)
    diagEmpty = a == 0 && d == 0
    antidiagEmpty = b == 0 && c == 0

mult :: Trans2dMat -> Trans2dMat -> Trans2dMat
mult (Mat a b c d) (Mat a' b' c' d') =
  Mat (a * a' + b * c') (a * b' + b * d') (c * a' + d * c') (c * b' + d * d')

det :: Trans2dMat -> Int
det (Mat a b c d) = a * d - b * c

instance Semigroup Trans2dMat where
  (<>) = mult

instance Monoid Trans2dMat where
  mempty = mk2dTransMat (1, 0, 0, 1)

instance Group Trans2dMat where
  inv m@(Mat a b c d) =
    mk2dTransMat
      ( d `div` mDet,
        (- b) `div` mDet,
        (- c) `div` mDet,
        a `div` mDet
      )
    where
      mDet = det m

{-
this time we'll define isomorphism by listing value pairs
then injective function (f') and its reverse (g') will use them
rotation transformations - anticlockwise
symmetry axis: horizontal, diagonal, vertical, antidiagonal
-}

iso :: [(D4, Trans2dMat)]
iso =
  [ (Dr0, mk2dTransMat (1, 0, 0, 1)),
    (Dr1, mk2dTransMat (0, -1, 1, 0)),
    (Dr2, mk2dTransMat (-1, 0, 0, -1)),
    (Dr3, mk2dTransMat (0, 1, -1, 0)),
    (Ds0, mk2dTransMat (1, 0, 0, -1)),
    (Ds1, mk2dTransMat (0, 1, 1, 0)),
    (Ds2, mk2dTransMat (-1, 0, 0, 1)),
    (Ds3, mk2dTransMat (0, -1, -1, 0))
  ]

f' :: D4 -> Trans2dMat
f' i = snd . fromJust . find ((== i) . fst) $ iso

g' :: Trans2dMat -> D4
g' m = fst . fromJust . find ((== m) . snd) $ iso

instance Semigroup D4 where
  i1 <> i2 = g' (f' i2 <> f' i1)

instance Monoid D4 where
  mempty = Dr0

instance Group D4 where
  inv = g' . inv . f'

{-
examples

>>> Dr1 <> Dr2
Dr3

>>> Dr3 <> Dr2 <> Dr1 <> Dr0
Dr2

>>> Dr1 <> Ds0
Ds3

>>> Ds0 <> Dr1
Ds1

>>> Ds0 <> Ds2
Dr2

>>> inv Dr1
Dr3

>>> inv Ds2
Ds2

-}

{-
C_4 in terms of matrices?
simply define monomorphism C4 -> Trans2dMat
and inversly - a partial function Trans2dMat -> C4 on rotating matrices

imagine scenario: you're given S_4 group (permutations of 4) and want to define D_4
you apply the idea above by carefully constructing monomorphism
in reverse mapping, use Maybe to ignore permutations not corresponding to any isometry
-}
