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

-- | functionality related to parsing tokens
module TokenParser ( module Text.ParserCombinators.Parsec
                   , variable
                   , lexer
                   , parens
                   , integer
                   , stringLiteral
                   , identifier
                   , float
                   , naturalOrFloat
                   , reservedOp
                   , reserved
                   , real_exp ) where


-- imports
import Text.ParserCombinators.Parsec 
import qualified Text.ParserCombinators.Parsec.Token as PT
import Text.ParserCombinators.Parsec.Language (haskellDef
                                              , reservedOpNames
                                              , reservedNames )

-- local imports
import CalculatorState



{- | some basic definitions for the calculator -}

-- | this is how valid variable names have to look like
variable :: CharParser CalcState String
variable = letter 
           >>= \first -> many alphaNum
           >>= \rest  -> return $ [first] ++ rest



{- | prepare needed parsers from Parsec.Token -}

-- | function generating a token parser based on a 
-- lexical parsers combined with a language record definition
lexer :: PT.TokenParser st
lexer  = PT.makeTokenParser 
         ( haskellDef { reservedOpNames = ["*","/","+","-","="]
                      , reservedNames   = ["sqrt"] } )


-- | token parser for parenthesis
parens :: CharParser st a -> CharParser st a
parens = PT.parens lexer


-- | token parser for Integer
integer :: CharParser st Integer
integer = PT.integer lexer


-- | token parser for Char
stringLiteral :: CharParser st String
stringLiteral = PT.stringLiteral lexer


-- | token parser for Char
identifier :: CharParser st String
identifier = PT.stringLiteral lexer


-- | token parser for Double
float :: CharParser st Double
float = PT.float lexer


-- | token parser for Either Integer Double
naturalOrFloat :: CharParser st (Either Integer Double)
naturalOrFloat = PT.naturalOrFloat lexer


-- | token parser for keywords
reservedOp :: String -> CharParser st ()
reservedOp = PT.reservedOp lexer


-- | token parser for keywords
reserved :: String -> CharParser st ()
reserved = PT.reserved lexer


-- | helper function for defining real powers
real_exp :: Double -> Double -> Double
real_exp a x = exp $ x * log a