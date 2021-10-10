module Circuit where

import Control.Monad.Fix
import Control.Monad
import GHC.Base
import Control.Category
import Control.Arrow
import Data.Functor
import Signal

newtype Circuit a b = Circuit {simulate :: Signal a -> Signal b}

instance Functor (Circuit a) where
    fmap f ca = Circuit $ \signal -> fmap f (ca `simulate` signal)

instance Applicative (Circuit a) where
    pure x = Circuit $ \_ -> Signal $ const x
    --cf <*> cs = Circuit $ \signal -> (cf `simulate` signal) >>= \f -> f <$> (cs `simulate` signal)
    (Circuit cf) <*> (Circuit cs) = Circuit $ \signal -> flip fmap (cs signal) =<< cf signal

instance Monad (Circuit a) where
    return = pure
    (Circuit c) >>= f = Circuit $ \signal -> (`simulate` signal) GHC.Base.. f =<< c signal

instance MonadFix (Circuit a) where
     mfix f = Circuit $ \signal -> mfix ((`simulate` signal) GHC.Base.. f)

instance Category Circuit where
    id = Circuit GHC.Base.id
    ca . cb = Circuit $ \signal -> ca `simulate` (cb `simulate` signal)

instance Arrow Circuit where
    arr f = Circuit $ fmap f
    first (Circuit ca) = Circuit $ \signal -> Signal $ \time -> (ca (Signal $ \t -> fst (signal `at` time)) `at` time, snd (signal `at` time))

instance ArrowLoop Circuit where
    loop (Circuit f) = Circuit (fmap fst GHC.Base.. mfix GHC.Base.. f')
        where f' x y = Signal $ \t -> (fst $ f (Signal $ \time -> (x `at` time, snd y `at` time)) `at` t, snd y)
