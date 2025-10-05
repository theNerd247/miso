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

-- Following properites of 
-- > type Effect parent model action = RWS (ComponentInfo parent) [Sink action -> JSM ()] model ()
-- 
-- - readonly on ComponentInfo parent
-- - write only on JSM effects (codensity over JSM, knot-tied)
-- - pure state transitions over model
--
-- The current miso library forces us to de-functionalize any state
-- transitions that occur as a result of asynchrous actions.
--
-- It would be nice to make Effect computations more composible so that
-- we can have more re-use.

-- | This is like JSM but suspends execution until it's given a @'Sink' a@ is given
newtype Act a = Act (Codensity JSM a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)
