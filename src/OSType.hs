-- | This module contains all the types used in the interpreter
module OSType where

import           Control.Monad.State
import qualified Data.Text                     as T

-- | List of text stored in registers
type Registers = [T.Text]

-- | List of instructions in the current program
type Instructions = [Instruction]

-- | Stateful data maintained during interpretation
data OSState = OSState { pos :: Int,
                         instrs :: Instructions,
                         regs :: Registers
                       }

-- | A One Sharp computation
type Instruction = State OSState ()
