module Signal where

import Control.Monad.Fix

type Time = Double
newtype Signal a = Signal {at :: Time -> a}

instance Functor Signal where
    fmap f (Signal a) = Signal $ \time -> f (a time)

instance Applicative Signal where
    pure x = Signal $ const x
    sf <*> (Signal a) = Signal $ \time -> (sf `at` time) (a time)

instance Monad Signal where
    return = pure
    sa >>= f = Signal $ \time -> f (sa `at` time) `at` time

instance MonadFix Signal where
    mfix f = Signal $ \time -> fix ((`at` time) . f)

instance Eq a => Eq (Signal a) where
    (Signal x) == (Signal y) = x 0 == y 0

instance Ord a => Ord (Signal a) where
    (Signal x) <= (Signal y) = x 0 <= y 0    

instance Num a => Num (Signal a) where
    sx + sy = Signal $ \time -> (sx `at` time) + (sy `at` time)
    sx * sy = Signal $ \time -> (sx `at` time) * (sy `at` time)
    abs sx = Signal $ \time -> abs $ sx `at` time
    fromInteger i = Signal $ const $ fromInteger i
    signum sx = Signal $ \time -> signum $ sx `at` time
    negate sx = Signal $ \time -> negate $ sx `at` time

instance Fractional a => Fractional (Signal a) where
    sx / sy = Signal $ \time -> (sx `at` time) / (sy `at` time)
    fromRational i = Signal $ const $ fromRational i
    
instance Show a => Show (Signal a) where
    show (Signal a) = show $ a 0
