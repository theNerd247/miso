{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Miso.Eff
  (
    
  ) where

-- import Miso.Effect (ComponentInfo)
-- import Control.Monad.Reader (ReaderT)
-- import Control.Monad.State (State)
import Miso.FFI.Internal (JSM)
import Control.Monad.Codensity (Codensity)
import Control.Monad.IO.Class (MonadIO)

-- type Effect parent model action = RWS (ComponentInfo parent) [Sink action -> JSM ()] model ()

-- | This is like JSM but suspends execution until it's given a @'Sink' a@.
newtype Act a = Act (Codensity JSM a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

type Effect parent model action = ReaderT (ComponentInfo parent) (StateT model)

-- r -> s -> (s, [m ()])
--
-- r -> s -> (s, m ())

-- Compose [] (Codensity JSM)
