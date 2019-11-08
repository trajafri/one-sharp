-- | This module contains all the types used in the interpreter
module OSType where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map                      as M
import qualified Data.Text                     as T

{-| A Parsed instruction
    Count represents the number of 1's
|-}
data ParsedInstr
  = WriteOne Int
  | WriteSharp Int
  | JumpForward Int
  | JumpBackwards Int
  | Cases Int deriving (Eq, Show)

-- | List of text stored in registers
type Registers = M.Map Int T.Text

-- | List of instructions in the current program
type Instructions = M.Map Int Instruction

-- | Stateful data maintained during interpretation
data OSState = OSState { pos :: Int,
                         instrs :: Instructions,
                         regs :: Registers
                       }

-- | A One Sharp computation
type Instruction = State OSState ()

type Program = ExceptT T.Text (State OSState) ()
