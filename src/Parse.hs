{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Utilities to parse one-sharp instructions
module Parse where

import           OSType
import qualified Data.Text                     as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- | Given a text representation of a single 1# instruction,
-- | converts it to a ParsedInstr
parseInstr :: T.Text -> ParsedInstr
parseInstr t = PI (countOnes t) (countSharps t)
 where
  countOnes :: T.Text -> Int
  countOnes = T.count "1"

  countSharps :: T.Text -> Int
  countSharps = T.count "#"

-- | Given a 1# program, converts it to a list of parsed instructions
-- | Currently, it doesn't parse comments.
collectInstrs :: Parsec Void T.Text [ParsedInstr]
collectInstrs = (:) . parseInstr <$> takeInstr <*> collectInstrs
 where
  takeInstr :: Parsec Void T.Text T.Text
  takeInstr = do
    ones   <- some $ space >> char '1'
    hashes <- some $ space >> char '#'
    return $ T.pack ones <> T.pack hashes
