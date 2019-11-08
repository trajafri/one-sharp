{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Utilities to parse one-sharp instructions
module Parse where
import           OSType
import           Data.Data
import qualified Data.Text                     as T
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- | Type for error when we receive more than 5 #'s
data SyntaxError = SE T.Text deriving (Eq, Data, Typeable, Ord, Read, Show)

instance ShowErrorComponent SyntaxError where
  showErrorComponent (SE txt) = "syntax error: " <> (T.unpack txt)

-- | Given a 1# program, converts it to a list of parsed instructions
-- | Currently, it doesn't parse comments.
-- | TODO: Throw error if we get more than 5 hashes
collectInstrs :: Parsec SyntaxError T.Text [ParsedInstr]
collectInstrs = (try (space >> eol >> return [])) <|> (space >> some takeInstr)
 where
  takeInstr :: Parsec SyntaxError T.Text ParsedInstr
  takeInstr = do
    ones   <- parseChars '1'
    hashes <- parseChars '#'
    let hLength = length hashes
    let oLength = length ones
    case hLength of
      1 -> return . WriteOne $ oLength
      2 -> return . WriteSharp $ oLength
      3 -> return . JumpForward $ oLength
      4 -> return . JumpBackwards $ oLength
      5 -> return . Cases $ oLength
      _ ->
        customFailure
          .  SE
          $  "expected at most 5 #'s, received "
          <> (T.pack . show $ hLength)

  parseChars :: Char -> Parsec SyntaxError T.Text [Char]
  parseChars ch =
    some
      $ (do
          c <- char ch
          space
          return c
        )

-- | Takes a 1# program in text and returns a 1# AST
parseOneSharp
  :: String
  -> T.Text
  -> Either (ParseErrorBundle T.Text SyntaxError) [ParsedInstr]
parseOneSharp fileName = runParser collectInstrs fileName
