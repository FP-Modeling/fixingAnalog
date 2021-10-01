module Circuit where

import Control.Monad.Fix
import Control.Monad
import Data.Functor
import Signal

infixr 5    @>

newtype Circuit a b = Circuit {simulate :: Signal a -> Signal b}

instance Functor (Circuit a) where
    fmap f ca = Circuit $ \signal -> fmap f (ca `simulate` signal)

instance Applicative (Circuit a) where
    pure x = Circuit $ \_ -> Signal $ const x
    --cf <*> cs = Circuit $ \signal -> (cf `simulate` signal) >>= \f -> f <$> (cs `simulate` signal)
    (Circuit cf) <*> (Circuit cs) = Circuit $ \signal -> flip fmap (cs signal) =<< cf signal

instance Monad (Circuit a) where
    return = pure
    (Circuit c) >>= f = Circuit $ \signal -> (`simulate` signal) . f =<< c signal

instance MonadFix (Circuit a) where
     mfix f = Circuit $ \signal -> mfix ((`simulate` signal) . f)

(@>) :: Circuit a b -> Circuit b c -> Circuit a c
(@>) c1 c2 = Circuit $ \signal -> c2 `simulate` (c1 `simulate` signal)
