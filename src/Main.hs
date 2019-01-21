{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

-- bytestring
-- import Data.ByteString (ByteString)

-- containers
-- import Data.Map.Strict (Map)

-- path
-- import Path

-- mk
import Options


-- newtype Var = Var { unVar :: ByteString }

-- type VarEvaluator = Path Abs File -> IO ByteString
-- -- type VarMap = Map Var VarEvaluator

-- data Chunk
--   = ChunkVerbatim !ByteString
--   | ChunkVar !Var

-- type RawTemplate = [Chunk]


main :: IO ()
main = do
  opts@Options{..} <- execOptionsParser
  print opts

  putStrLn "TODO"
  -- TODO
  -- Find matching template file
  -- Parse template
  -- Evaluate variable
  -- Write new file
  -- TODO: Check if new file already exists, only overwrite if given flag "-f/--force"
