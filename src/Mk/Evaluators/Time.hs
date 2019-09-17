{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Mk.Evaluators.Time
  ( evalDAY
  , evalMONTH
  , evalYEAR
  , evalDATE
  , evalTIME
  , evalFDATE
  ) where

-- base
import Control.Monad.IO.Class

-- text
import Data.Text (Text)

-- time
import Data.Time.Format
import Data.Time.LocalTime

-- mk
import Mk.Evaluators.Types


-- | The current local time in the given format.
-- See @Data.Time.Format@ for a description of the format string syntax.
formatCurrentLocalTime :: MonadIO m => String -> m String
formatCurrentLocalTime fmt = do
  zlt <- liftIO getZonedTime
  pure (formatTime defaultTimeLocale fmt zlt)

evalLocalTimeWithFormat :: MonadEvaluator m => String -> Var -> Text -> EvaluatorInfo m
evalLocalTimeWithFormat fmt intendedVar description = mkEvalInfo intendedVar description action
  where action = formatCurrentLocalTime fmt

evalDAY :: MonadEvaluator m => EvaluatorInfo m
evalDAY = evalLocalTimeWithFormat "%d" (Var "DAY") description
  where description = "Current day of the month as numeric value."
{-# INLINABLE evalDAY #-}

evalMONTH :: MonadEvaluator m => EvaluatorInfo m
evalMONTH = evalLocalTimeWithFormat "%m" (Var "MONTH") description
  where description = "Current month of the year as numeric value."
{-# INLINABLE evalMONTH #-}

evalYEAR :: MonadEvaluator m => EvaluatorInfo m
evalYEAR = evalLocalTimeWithFormat "%0Y" (Var "YEAR") description
  where description = "Current year as numeric value."
{-# INLINABLE evalYEAR #-}

evalDATE :: MonadEvaluator m => EvaluatorInfo m
evalDATE = evalLocalTimeWithFormat "%0Y-%m-%d" (Var "DATE") description
  where description = "Current date in 'YYYY-mm-dd' format. "
                      <> "This is equivalent to expanding '%YEAR%-%MONTH%-%DAY%'."
{-# INLINABLE evalDATE #-}

evalTIME :: MonadEvaluator m => EvaluatorInfo m
evalTIME = evalLocalTimeWithFormat "%H:%M" (Var "TIME") description
  where description = "Current time in 'HH:MM' format."
{-# INLINABLE evalTIME #-}

evalFDATE :: MonadEvaluator m => EvaluatorInfo m
evalFDATE = evalLocalTimeWithFormat "%0Y-%m-%d %H:%M" (Var "FDATE") description
  where description = "Current full date (date and time) in 'YYYY-mm-dd HH:MM' format. "
                      <> "This is equivalent to expanding '%DATE% %TIME%'."
{-# INLINABLE evalFDATE #-}
