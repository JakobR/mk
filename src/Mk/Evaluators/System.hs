{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Mk.Evaluators.System
  ( evalHOST
  , evalUSER
  ) where


-- base
import Control.Monad.IO.Class
import Data.Char (isSpace)

-- hostname
import Network.HostName (getHostName)

-- unix
import System.Posix.User (getRealUserID, UserEntry(..), getUserEntryForID)

--mk
import Mk.Evaluators.Types


evalHOST :: MonadEvaluator m => EvaluatorInfo m
evalHOST = mkEvalInfo (Var "HOST") description action
  where
    description = "Current host name."
    action = liftIO getHostName
{-# INLINABLE evalHOST #-}

evalUSER :: MonadEvaluator m => EvaluatorInfo m
evalUSER = mkEvalInfo (Var "USER") description action
  where
    description = "Name of the currently logged-in user."
    action = liftIO $ do
      uid <- getRealUserID
      UserEntry{..} <- getUserEntryForID uid
      return (if not (isEmpty userGecos) then userGecos else userName)
    isEmpty = all isSpace
{-# INLINABLE evalUSER #-}
