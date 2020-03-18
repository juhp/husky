module Number (
  Number(..),
  numOp
  )
where

data Number = NumInt Integer | NumReal Double
  deriving Eq

instance Show Number where
  show (NumInt i) = show i
  show (NumReal r) = show r

instance Ord Number where
  (<=) = numOperator (<=) (<=)

instance Num Number where
  n1 + n2 = numOp (+) (+) n1 n2
  n1 * n2 = numOp (*) (*) n1 n2
  n1 - n2 = numOp (-) (-) n1 n2
  abs = numMorph abs abs
  signum = numMorph signum signum
  fromInteger = NumInt

instance Fractional Number where
  fromRational = NumReal . fromRational
  n1 / n2 = realOp (/) n1 n2

instance Floating Number where
  pi = NumReal pi
  exp = numRealFn exp
  log  = numRealFn log
  sin = numRealFn sin
  cos = numRealFn cos
  asin = numRealFn asin
  acos = numRealFn acos
  atan = numRealFn atan
  sinh = numRealFn sinh
  cosh = numRealFn cosh
  asinh = numRealFn asinh
  acosh = numRealFn acosh
  atanh = numRealFn atanh

instance Real Number where
  toRational (NumInt i) = toRational i
  toRational (NumReal r) = toRational r

instance RealFrac Number where
  properFraction n =
    NumReal <$>  properFraction (toReal n)

toReal :: Number -> Double
toReal (NumInt i) = fromInteger i
toReal (NumReal r) = r

numMorph :: (Integer -> Integer) -> (Double -> Double) -> Number -> Number
numMorph f _ (NumInt i) = NumInt $ f i
numMorph _ f (NumReal r) = NumReal $ f r

numRealFn :: (Double -> Double) -> Number -> Number
numRealFn f = NumReal . f . toReal

numOperator :: (Integer -> Integer -> a) -> (Double -> Double -> a) -> Number -> Number -> a
numOperator op _ (NumInt i1) (NumInt i2) = op i1 i2
numOperator _ op (NumReal r1) (NumReal r2) = op r1 r2
numOperator _ op (NumInt i) (NumReal r) = op (fromInteger i) r
numOperator _ op (NumReal r) (NumInt i) = op r (fromInteger i)

numOp :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> Number -> Number -> Number
numOp op _ (NumInt i1) (NumInt i2) = NumInt $ op i1 i2
numOp _ op (NumReal r1) (NumReal r2) = NumReal $ op r1 r2
numOp _ op (NumInt i) (NumReal r) = NumReal $ op (fromInteger i) r
numOp _ op (NumReal r) (NumInt i) = NumReal $ op r (fromInteger i)

realOp :: (Double -> Double -> Double) -> Number -> Number -> Number
realOp op n1 n2 = NumReal $ op (toReal n1) (toReal n2)
