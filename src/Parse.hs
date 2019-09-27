{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Utilities to parse one-sharp instructions
module Parse where

import           OSType
import qualified Data.Text                     as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- | Given a 1# program, converts it to a list of parsed instructions
-- | Currently, it doesn't parse comments.
-- | TODO: Throw error if we get more than 5 hashes
collectInstrs :: Parsec Void T.Text [ParsedInstr]
collectInstrs = (try (space >> eol >> return [])) <|> (space >> some takeInstr)
 where
  takeInstr :: Parsec Void T.Text ParsedInstr
  takeInstr = do
    ones <-
      some
        $ (do
            c <- char '1'
            space
            return c
          )
    hashes <-
      some
        $ (do
            c <- char '#'
            space
            return c
          )
    return . PI (length ones) $ length hashes

-- | Takes a 1# program in text and returns a 1# AST
parseOneSharp
  :: String -> T.Text -> Either (ParseErrorBundle T.Text Void) [ParsedInstr]
parseOneSharp fileName = runParser collectInstrs fileName
