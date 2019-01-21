module Mk.Char8
  ( ord8
  , chr8
  , isAlphaNum8
  , newline8
  ) where


-- base
import Data.Char (chr, isAlphaNum, ord)
import Data.Word (Word8)

-- conversion
import Conversion (convert)


ord8 :: Char -> Word8
ord8 c =
  case convert (ord c) of
    Just x -> x
    Nothing -> error $ "cannot to convert Char " <> show c <> " to Word8"
{-# INLINABLE ord8 #-}

chr8 :: Word8 -> Char
chr8 = chr . convert
{-# INLINABLE chr8 #-}

isAlphaNum8 :: Word8 -> Bool
isAlphaNum8 = isAlphaNum . chr8
{-# INLINABLE isAlphaNum8 #-}

newline8 :: Word8
newline8 = ord8 '\n'
{-# INLINABLE newline8 #-}
