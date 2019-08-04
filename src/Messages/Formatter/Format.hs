module Messages.Formatter.Format
  ( InfoFormatter
  , InfoFormatterF(..)
  , onInfo
  , approve
  )
where

import           Control.Monad.Free
import           Messages.Types


class Functor f => InfoFormatter f where
    onInfo :: InfoMessage -> f ()
    approve :: PromptMessage -> f Bool


data InfoFormatterF a
    = OnInfo InfoMessage a
    | Approve PromptMessage (Bool -> a)


instance InfoFormatter InfoFormatterF where
  onInfo info = OnInfo info ()
  approve prompt = Approve prompt id


instance Functor InfoFormatterF where
  fmap f (OnInfo  info   a) = OnInfo info (f a)
  fmap f (Approve prompt a) = Approve prompt (f . a)


instance InfoFormatter f => InfoFormatter (Free f) where
  onInfo info = liftF (onInfo info)
  approve prompt = liftF (approve prompt)
