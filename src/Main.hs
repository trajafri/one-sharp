{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE MultiWayIf #-}

module Main where

import           Control.Monad.State
import qualified Data.Text                     as T
import qualified Data.Map                      as M
import           OSType
import           Parse
import           Text.Megaparsec

{---------------------------------------------------------------------------
 1# Instructions
 ---------------------------------------------------------------------------}

-- | Creates a 1# instruction to `1` to register n
writeOne :: Int -> Instruction
writeOne n = do
  modify' $ \(OSState p is rs) ->
    OSState (p + 1) is $ M.insertWithKey (const . flip $ (<>)) n "1" rs

-- | Creates a 1# instruction to `#` to register n
writeHash :: Int -> Instruction
writeHash n = do
  modify' $ \(OSState p is rs) ->
    OSState (p + 1) is $ M.insertWithKey (const . flip $ (<>)) n "#" rs

-- | Creates a 1# instruction to jump `n` steps forward
jumpForward :: Int -> Instruction
jumpForward n = do
  modify' $ \(OSState p is rs) -> OSState (p + n) is rs

-- | Creates a 1# instruction to jump `n` steps backwards 
jumpBackwards :: Int -> Instruction
jumpBackwards n = do
  modify' $ \(OSState p is rs) -> OSState (p - n) is rs

-- | Creates a 1# instruction to do cases on the `n`th instruction
cases :: Int -> Instruction
cases n = do
  (OSState p is rs) <- get
  let registerText = maybe "" id $ M.lookup n rs
  case T.uncons registerText of
    Nothing -> put (OSState (p + 1) is rs)
    Just ('1', rest) ->
      put (OSState (p + 2) is $ M.update (const . Just $ rest) n rs)
    Just ('#', rest) ->
      put (OSState (p + 3) is $ M.update (const . Just $ rest) n rs)
    _ -> noop

-- | Creates a 1# instruction that does nothing. This is only used to
-- | avoid non-exhaustive patterns
noop :: Instruction
noop = return ()

{---------------------------------------------------------------------------
 Parsed 1# Instruction helpers
 ---------------------------------------------------------------------------}

-- | If instruction is invalid, creates an error.
verifyInstruction :: ParsedInstr -> Either T.Text ParsedInstr
verifyInstruction p@(PI os hs)
  |
 --This is only possible if the whole program starts with #
    os < 0
  = Left "Program doesn't begin with 1's"
  | hs < 0
  = Left $ "Instruction at position " <> "" <> " doesn't end with #'s"
  | hs > 5
  = Left
    $  "Instruction at position "
    <> ""
    <> " doesn't contains more than five #'s"
  | otherwise
  = return p

-- | Converts a parsed 1# instruction to a 1# computation.
piToInstr :: ParsedInstr -> Instruction
piToInstr (PI os hs) =
  case hs of
      1 -> writeOne
      2 -> writeHash
      3 -> jumpForward
      4 -> jumpBackwards
      5 -> cases
      _ -> const noop
    $ os

-- | Adds all instructions to memory and creates a 1# program
-- | Assumes that all registers are empty
pisToInstrs :: [ParsedInstr] -> Program
pisToInstrs pis = put $ OSState 1 (piToInstr <$> pis) M.empty

{---------------------------------------------------------------------------
 1# Interpreter
 ---------------------------------------------------------------------------}

eval :: Program
eval = do
  (OSState p is _) <- get
  if
    | p == (length is + 1)         -> return ()
    | --Should produce an error here
      p < 0 || p > (length is + 1) -> return ()
    | otherwise                    -> is !! (p - 1) >> eval

-- | Temporary main to test programs in file named `test`
main :: IO ()
main = do
  fileContent <- readFile "testos"
  let listOfPinstr = runParser collectInstrs "testos" $ T.pack fileContent
  case listOfPinstr of
    Left  e   -> error $ "It failed brother\n" <> errorBundlePretty e
    Right pis -> do
      let (_, st) =
            (`runState` (OSState 0 [] M.empty)) $ pisToInstrs pis >> eval
      putStrLn . show . regs $ st
