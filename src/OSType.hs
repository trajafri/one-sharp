-- | This module contains all the types used in the interpreter
module OSType where

import           Control.Monad.State
import qualified Data.Map                      as M
import qualified Data.Text                     as T

{-| A Parsed instruction
    First count represents the number of 1's
    Second count represents the number of #'s
|-}
data ParsedInstr = PI Int Int

-- | List of text stored in registers
type Registers = M.Map Int T.Text

-- | List of instructions in the current program
type Instructions = [Instruction]

-- | Stateful data maintained during interpretation
data OSState = OSState { pos :: Int,
                         instrs :: Instructions,
                         regs :: Registers
                       }

-- | A One Sharp computation
type Instruction = State OSState ()

type Program = Instruction
