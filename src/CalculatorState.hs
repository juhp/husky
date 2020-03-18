{-----------------------------------------------------------------

  (c) 2008-2009 Markus Dittrich

  This program is free software; you can redistribute it
  and/or modify it under the terms of the GNU General Public
  License Version 3 as published by the Free Software Foundation.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License Version 3 for more details.

  You should have received a copy of the GNU General Public
  License along with this program; if not, write to the Free
  Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.

--------------------------------------------------------------------}

-- | this module contains small bits and pieces needed for
-- the calculator. Eventually, these might all find a home
-- in their separate modules
module CalculatorState ( CalcState(..)
                       , clear_stack
                       , defaultCalcState
                       , Function(..)
                       , insert_function
                       , insert_variable
                       , push_to_stack
                       ) where


-- imports
import qualified Data.Map as M
import Prelude

import Number (Number(..))

-- | this data structure holds information for user defined
-- functions
data Function = Function
                {
                  f_vars :: [String]
                , f_expression :: String
                }
              deriving(Show)

-- | this data structure provides some state information
-- to the calculator (variables, etc ...)
-- Currently, we thread the following pieces of information:
-- varMap   : map with all currently defined variable/value pairs
--            (persistent across calls to reset_state below)
-- errState : bool indicating that any special error messages
--            have been queued
-- errValue : [String] holding all special error messages
data CalcState = CalcState
    {
      varMap    :: M.Map String Number
    , funcMap   :: M.Map String Function
    , funcStack :: M.Map String Number   -- local stack for passing
                                         -- function parameters
    }


defaultCalcState :: CalcState
defaultCalcState = CalcState
    {
      varMap    = M.fromList $ map (fmap NumReal) constantList
    , funcMap   = M.empty
    , funcStack = M.empty
    }



-- | function pushing a variable on the stack
push_to_stack :: (String,Number) -> CalcState -> CalcState
push_to_stack (name,val) state@CalcState {funcStack = stack} =
    state { funcStack = M.insert name val stack }


-- | function for clearing the stack
clear_stack :: CalcState -> CalcState
clear_stack state = state { funcStack = M.empty }


-- | function adding a new variable into the calculator state
-- database
insert_variable :: String -> Number -> CalcState -> CalcState
insert_variable name num state@CalcState {varMap = theMap} =
    state { varMap = M.insert name num theMap }


-- | insert a user definied function into the calculator state
-- database
insert_function :: String -> [String] -> String -> CalcState
                -> CalcState
insert_function name vars expr state@CalcState {funcMap = theMap} =
    state { funcMap = M.insert name theFunc theMap }
  where
    theFunc = Function { f_vars = vars, f_expression = expr }


-- | provide a few useful mathematical constants that we
-- load into the default CalculatorState
constantList :: [(String,Double)]
constantList =
    [ ("pi",3.14159265358979323846264338327950288)
    , ("e",2.71828182845904523536028747135266249)
    , ("phi",1.61803398874989484820458683436563811)]  -- golden ratio
