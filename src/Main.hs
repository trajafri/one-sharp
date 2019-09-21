{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.State
import qualified Data.Text                     as T
import qualified Data.Map                      as M
import           OSType

{---------------------------------------------------------------------------
 1# Instructions
 ---------------------------------------------------------------------------}

-- | Creates a 1# instruction to `1` to register n
writeOne :: Int -> Instruction
writeOne n = do
  modify' $ \(OSState p is rs) ->
    OSState p is $ M.insertWithKey (const (<>)) n "1" rs

-- | Creates a 1# instruction to `#` to register n
writeHash :: Int -> Instruction
writeHash n = do
  modify' $ \(OSState p is rs) ->
    OSState p is $ M.insertWithKey (const (<>)) n "#" rs

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
    Nothing       -> put (OSState (p + 1) is rs)
    Just ('1', _) -> put (OSState (p + 2) is rs)
    Just ('#', _) -> put (OSState (p + 3) is rs)
    _             -> noop

-- | Creates a 1# instructino that does nothing. This is only used to
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



{---------------------------------------------------------------------------
 1# Instructions
 ---------------------------------------------------------------------------}






main :: IO ()
main = putStrLn "Hello, Haskell!"
