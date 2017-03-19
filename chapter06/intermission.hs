data TisAnInteger =
    TisAn Integer

instance Eq (TisAnInteger) where
  (==) (TisAn a) (TisAn a') = a == a'

data TwoIntegers = Two Integer Integer

instance Eq (TwoIntegers) where
  (==) (Two a b) (Two a' b') = (a == a') && (b == b')

data StringOrInt =
    TisAnInt    Int
  | TisAString  String

instance Eq (StringOrInt) where
    (==) (TisAnInt a) (TisAnInt b) = a == b
    (==) (TisAString a) (TisAString b) = a == b
    (==) _ _ = False

data Pair a =
    Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair a a') (Pair b b') = (a == b) && (a' == b')

data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = (a == a') && (b == b')

data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq(Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) (_) (_) = False

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq(EitherOr a b) where
    (==) (Hello a) (Hello b) = a == b
    (==) (Goodbye a) (Goodbye b) = a == b
    (==) (_) (_) = False
