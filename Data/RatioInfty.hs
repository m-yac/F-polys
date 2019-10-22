
{- A modified version of GHC.Real which allows for infnity as a valid rational number -}
module Data.RatioInfty ( Ratio , Rational, (%) , numerator , denominator , numerator' , denominator' , infinity ) where

import Prelude hiding (Rational)
import qualified Data.Ratio as R

--------------------------------------------------------------
-- The Ratio and Rational types
--------------------------------------------------------------

-- | Rational numbers, with numerator and denominator of some 'Integral' type.
--
-- Note that `Ratio`'s instances inherit the deficiencies from the type
-- parameter's. For example, @Ratio Natural@'s 'Num' instance has similar
-- problems to `Numeric.Natural.Natural`'s.
data  Ratio a = !a :% !a  deriving Eq -- ^ @since 2.01

type  Rational          =  Ratio Int

ratioPrec, ratioPrec1 :: Int
ratioPrec  = 7  -- Precedence of ':%' constructor
ratioPrec1 = ratioPrec + 1

infinity, notANumber :: Integral a => Ratio a
infinity   = 1 :% 0
notANumber = 0 :% 0

-- Use :%, not % for NaN; the latter would
-- immediately lead to a runtime error, because it normalises.

-- | Forms the ratio of two integral numbers.
(%)                     :: (Integral a) => a -> a -> Ratio a

-- | Extract the numerator of the ratio in reduced form:
-- the numerator and denominator have no common factor and the denominator
-- is positive.
numerator       :: Ratio a -> a

-- | Extract the denominator of the ratio in reduced form:
-- the numerator and denominator have no common factor and the denominator
-- is positive.
denominator     :: Ratio a -> a


-- | 'reduce' is a subsidiary function used only in this module.
-- It normalises a ratio by dividing both numerator and denominator by
-- their greatest common divisor.
reduce ::  (Integral a) => a -> a -> Ratio a
reduce x y              =  (x `quot` d) :% (y `quot` d)
                           where d = gcd x y

0 % 0                   =  (0 R.% 0) `seq` notANumber
x % 0                   =  infinity
x % y                   =  reduce (x * signum y) (abs y)

numerator   (x :% _)    =  x
denominator (_ :% y)    =  y

numerator'       :: (Integral a) => Ratio a -> a
denominator'     :: (Integral a) => Ratio a -> a
numerator'   (x :% _)   = abs x
denominator' (x :% y)   = y * signum x

--------------------------------------------------------------
-- Instances for @Ratio@
--------------------------------------------------------------

-- | @since 2.0.1
instance  (Integral a)  => Ord (Ratio a)  where
    (x:%y) <= (x':%y')  =  x * y' <= x' * y
    (x:%y) <  (x':%y')  =  x * y' <  x' * y

-- | @since 2.0.1
instance  (Integral a)  => Num (Ratio a)  where
    (x:%y) + (x':%y')   =  reduce (x*y' + x'*y) (y*y')
    (x:%y) - (x':%y')   =  reduce (x*y' - x'*y) (y*y')
    (x:%y) * (x':%y')   =  reduce (x * x') (y * y')
    negate (x:%y)       =  (-x) :% y
    abs (x:%y)          =  abs x :% y
    signum (x:%_)       =  signum x :% 1
    fromInteger x       =  fromInteger x :% 1

-- | @since 2.0.1
instance  (Integral a)  => Fractional (Ratio a)  where
    (x:%y) / (x':%y')   =  (x*y') % (y*x')
    recip (x:%y)
        | x < 0         = negate y :% negate x
        | otherwise     = y :% x
    fromRational q =  fromInteger (R.numerator q) % fromInteger (R.denominator q)

-- | @since 2.0.1
instance  (Integral a)  => Real (Ratio a)  where
    toRational (x:%y)   =  toInteger x R.% toInteger y

-- | @since 2.0.1
instance  (Integral a)  => RealFrac (Ratio a)  where
    properFraction (x:%y) = (fromInteger (toInteger q), r:%y)
                          where (q,r) = quotRem x y

-- | @since 2.0.1
instance  (Show a, Integral a)  => Show (Ratio a)  where
    showsPrec p (_:%0)  =  showString "infinity"
    showsPrec p (x:%1)  =  showsPrec ratioPrec1 x
    showsPrec p (x:%y)  =  showParen (p > ratioPrec) $
                           (if x < 0 then showString "-" else id) .
                           showsPrec ratioPrec1 (abs x) .
                           showString "/" .
                           showsPrec ratioPrec1 y

-- -- | @since 2.0.1
-- instance  (Integral a)  => Enum (Ratio a)  where
--     succ x              =  x + 1
--     pred x              =  x - 1

--     toEnum n            =  fromIntegral n :% 1
--     fromEnum            =  fromInteger . truncate

--     enumFrom            =  numericEnumFrom
--     enumFromThen        =  numericEnumFromThen
--     enumFromTo          =  numericEnumFromTo
--     enumFromThenTo      =  numericEnumFromThenTo



