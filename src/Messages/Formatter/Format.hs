module Messages.Formatter.Format (InfoFormatter, InfoFormatterF(..), onInfo) where

import Control.Monad.Free
import Messages.Types


class Functor f => InfoFormatter f where
    onInfo :: InfoMessage -> f ()


data InfoFormatterF a
    = OnInfo InfoMessage a


instance InfoFormatter InfoFormatterF where
    onInfo info = OnInfo info ()


instance Functor InfoFormatterF where
    fmap f (OnInfo info a) = OnInfo info (f a)


instance InfoFormatter f => InfoFormatter (Free f) where
    onInfo info = liftF (onInfo info)
