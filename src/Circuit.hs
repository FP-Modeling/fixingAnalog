module Circuit where

import Control.Monad.Fix
import Control.Monad
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
    (Circuit mc) >>= f = Circuit $ \signal -> (`simulate` signal) . f =<< mc signal

instance MonadFix (Circuit a) where
     mfix f = Circuit $ \signal -> mfix ((`simulate` signal) . f)

-- instance Show a => Show (Circuit a) where
--     show (Circuit a)     = show a