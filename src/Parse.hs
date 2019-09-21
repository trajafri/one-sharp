{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Utilities to parse one-sharp instructions
module Parse where

import           OSType
import qualified Data.Text                     as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

{-| A Parsed instruction
    First count represents the number of 1's
    Second count represents the number of #'s
|-}
data ParsedInstr = PI Int Int

-- | Given a text representation of a single 1# instruction,
-- | converts it to a ParsedInstr
parseInstr :: T.Text -> ParsedInstr
parseInstr t = PI (countOnes t) (countSharps t)
 where
  countOnes :: T.Text -> Int
  countOnes t = T.count "1" t

  countSharps :: T.Text -> Int
  countSharps t = T.count "#" t

-- | Given a 1# program, converts it to a list of parsed instructions
-- | Currently, it doesn't parse comments.
collectInstrs :: Parsec Void T.Text [ParsedInstr]
collectInstrs = do
  instr  <- takeInstr
  instrs <- collectInstrs
  return $ parseInstr instr : instrs
 where
  takeInstr :: Parsec Void T.Text T.Text
  takeInstr = do
    ones   <- some $ space >> char '1'
    hashes <- some $ space >> char '#'
    return $ T.pack ones <> T.pack hashes
