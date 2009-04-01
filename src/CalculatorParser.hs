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

-- | main archy driver
module CalculatorParser ( calculator_parser ) where


-- imports
import qualified Data.Map as M
import Control.Monad

-- local imports
import CalculatorState
import ExtraFunctions
import Prelude
import TokenParser

--import Debug.Trace

-- | grammar description for calculator parser
calculator_parser :: CharParser CalcState ParseResult
calculator_parser = parse_statements <* eof 
                 <?> "math expression, variable definition, " 
                     ++ "variable name"


-- | parse individual statements separated by semicolon
-- NOTE: 'sepBy1' as opposed to 'sepBy' is crucial here to
-- guarantee the list is not empty; otherwise head will die
-- on us.
parse_statements :: CharParser CalcState ParseResult
parse_statements = (head . reverse) <$> individual_statement 
                   `sepBy1` semi
                <?> "statement"
             

-- | parse an individual statement, i.e. either a computation
-- , a function definition, or a variable definition
individual_statement :: CharParser CalcState ParseResult
individual_statement = try define_function
                    <|> try (DblResult <$> define_variable) 
                    <|> (DblResult <$> add_term) 
                    <?> "expression or variable definition"



-- | if the line starts off with a string we either
-- have a variable definition or want to show the value
-- stored in a variable
define_variable :: CharParser CalcState Double
define_variable = variable_def_by_value 
               <?> "variable definition"


-- | define a variable via a literal double
variable_def_by_value :: CharParser CalcState Double
variable_def_by_value = update_var (whiteSpace *> variable) 
   (whiteSpace *> reservedOp "=" *> whiteSpace *> add_term)
                     <?> "variable from value"


-- | update the state of a variable
update_var :: CharParser CalcState String 
           -> CharParser CalcState Double
           -> CharParser CalcState Double
update_var name_p val_p = name_p
       >>= \name -> val_p
       >>= \val  -> updateState (insert_variable val name)
       >> return val


-- | parser for expressions chained via "+" or "-"
add_term :: CharParser CalcState Double
add_term = mul_term `chainl1` add_action


-- | parser for expressions chained via "*" or "/"
mul_term :: CharParser CalcState Double
mul_term = exp_term `chainl1` multiply_action


-- | parser for potentiation operations "^"
exp_term :: CharParser CalcState Double
exp_term = (whiteSpace *> factor) `chainl1` exp_action


-- | parser for individual factors, i.e, numbers,
-- variables or operations
factor :: CharParser CalcState Double
factor = try signed_parenthesis
      <|> try parse_user_functions
      <|> parse_functions
      <|> parse_functions_int 
      <|> try parse_single_number  -- need try because of possible
                                   -- unitary '-'
      <|> try parse_stack          -- always parse stack first since
                                   -- local variables hide global ones
      <|> parse_variable           
      <?> "token or variable"         


-- | parse a potentially signed expression enclosed in parenthesis.
-- In the case of parenzised expressions we parse -() as (-1.0)*()
signed_parenthesis :: CharParser CalcState Double
signed_parenthesis = (*) <$> parse_sign <*> parens add_term


-- | parse all operations of type (Double -> Double)
-- we currently know about
parse_functions :: CharParser CalcState Double
parse_functions = msum $ extract_ops builtinFunctions

  where
    extract_ops = foldr (\(x,y) acc -> 
                         ((reserved x *> execute y):acc)) [] 
    execute op  =  op <$> (  parens add_term 
                         <|> parse_single_number
                         <|> parse_variable )
               <?> "function parsing"


-- | parse all operations of type (Int -> Int) we currently know about
-- NOTE: They way we do things right now to deal with Integers
-- in the framework of our Double parser is somewhat of a 
-- hack. In a nutshell, we check if a Double can be interpreted
-- as an Integer and then use (of fail the parse)
parse_functions_int :: CharParser CalcState Double
parse_functions_int = msum $ extract_ops_int builtinFunctionsInt

  where
    extract_ops_int :: [(String, Integer -> Integer)] 
                    -> [CharParser CalcState Double]
    extract_ops_int = foldr (\(x,y) acc -> 
                             ((reserved x *> execute_int y):acc)) []

    execute_int op  =  fromInteger . op <$> (  parens add_term 
                                           <|> parse_single_number 
                                           <|> parse_variable 
                                               >>= evaluate_int )

    evaluate_int =  \val -> case is_non_negative_int val of
                              Just a -> return a
                              Nothing -> pzero 
                    <?> "non-negative integer value" 


-- | chain multiplicative of divisive statements
multiply_action :: CharParser CalcState (Double -> Double -> Double)
multiply_action = (reservedOp "*" *> pure (*))
               <|> (reservedOp "/" *> pure (/))


-- | chain additive or subtractive statements
add_action :: CharParser CalcState (Double -> Double -> Double)
add_action = (reservedOp "+" *> pure (+))
          <|> (reservedOp "-" *> pure (-))


-- | parse an exponentiation term
exp_action :: CharParser CalcState (Double -> Double -> Double)
exp_action = reservedOp "^" *> pure real_exp


-- | parse a single number; integers are automatically promoted 
-- to double
-- NOTE: Due to the notFollowedBy this parser can not be used
-- with 'many' and other parser combinators.
parse_single_number :: CharParser CalcState Double
parse_single_number = parse_number <* notFollowedBy alphaNum
            <?> "signed single integer or double"


-- | parse a number, can be used with 'many' and other parser
-- combinators; integers are automatically promoted to double
parse_number :: CharParser CalcState Double
parse_number = converter <$> (parse_sign <* whiteSpace) <*> 
               naturalOrFloat 
            <?> "signed integer or double"
  where 
    converter sign val = case val of
                           Left i  -> sign * (fromInteger i)
                           Right x -> sign * x


-- | parse the sign of a numerical expression
parse_sign :: CharParser CalcState Double
parse_sign = option 1.0 ( whiteSpace *> char '-' *> pure (-1.0) )


-- | look for the value of a given variable if any
parse_variable :: CharParser CalcState Double
parse_variable = (*) <$> (parse_sign <* whiteSpace) <*>
                 (get_variable_value variable <* whiteSpace)
              <?> "variable"


-- | function retrieving a variable from the database if
-- present 
get_variable_value :: CharParser CalcState String 
                   -> CharParser CalcState Double
get_variable_value name_parser = getState 
  >>= \(CalcState { varMap = myMap }) -> name_parser
  >>= \name -> case M.lookup name myMap of
                 Nothing -> pzero
                 Just a  -> return a
                            

-- | this is how valid variable names have to look like
variable :: CharParser CalcState String
variable = (:) <$> letter <*> many alphaNum


-- | look for the value of a given stack variable
parse_stack :: CharParser CalcState Double
parse_stack = (*) <$> (parse_sign <* whiteSpace) <*>
                 (get_stack_variable variable <* whiteSpace)
              <?> "variable"


-- | function retrieving a variable from the database if
-- present 
get_stack_variable :: CharParser CalcState String 
                   -> CharParser CalcState Double
get_stack_variable name_parser = getState 
  >>= \(CalcState { funcStack = stack }) -> name_parser
  >>= \name -> case M.lookup name stack of
                 Nothing -> pzero
                 Just a  -> return a
                            

-- | this is how valid function Strings have to look like
functionString :: CharParser CalcState String
functionString = many anyChar


-- | parser for a function definition
-- TODO: It might be a good idea to check the user defined
-- function somewhat, e.g., do the parameters match etc
define_function :: CharParser CalcState ParseResult
define_function = add_function parse_function_name parse_vars 
                  parse_function_def
              
  where
    add_function name_parser var_parser expr_parser = name_parser
      >>= \name -> var_parser 
      >>= \vars -> expr_parser
      >>= \expr -> updateState (insert_function vars expr name)
      >> return (StrResult "<function>") 
    
    parse_function_name = (whiteSpace *> reserved "function" 
                           *> whiteSpace *> variable <* whiteSpace)

    -- | we allow both f(x,y) and haskell style f x y function 
    -- definitions
    parse_vars = (parens ((variable <* whiteSpace) `sepBy` comma))
              <|> many (variable <* whiteSpace)

    parse_function_def = (whiteSpace *> reservedOp "=" *> whiteSpace 
                          *> functionString)


-- | parse available user function; the way we deal with user
-- functions for now goes like this:
--  1) Check if a user function of the given name exists
--  2) If yes, check if the user supplied the proper number of
--     arguments (for now we only allow literals, not variables)
--  3) If yes, replace the variables by the literals in the function
--     string.
--  4) Insert the so manipulated and parenthesized function 
--     expression into the current parser and parse it
parse_user_functions :: CharParser CalcState Double 
parse_user_functions = 
  substitute_function parse_function_name parse_arguments

  where
    -- | arguments can either be applied via f(x,y) or the haskell
    -- way f x y
    parse_arguments = parens ((parse_number <* whiteSpace) `sepBy` 
                              comma) 
                   <|> many (parse_number <* whiteSpace)

    parse_function_name = (whiteSpace *> variable <* whiteSpace)


    -- | substitute a function expression into the current parse
    -- string
    substitute_function name_parser var_parser = name_parser
      >>= get_function_expression
      >>= \(Function { f_vars = target_vars
                     , f_expression = expr } ) -> var_parser
      >>= \vars -> push_vars_to_stack vars target_vars
      >> getInput
      >>= \inp  -> setInput ("(" ++ expr ++ ")" ++ inp)
      >> parens add_term 
      >>= \result -> updateState clear_stack
      >> return result
 
                    
    -- | retrieve the function expression corresponding to a
    -- particular function name
    get_function_expression name = getState
      >>= \(CalcState { funcMap = myMap }) -> 
          case M.lookup name myMap of
            Nothing -> pzero
            Just a  -> return a

    -- | check if the number of expected and provided arguments
    -- match and push the variables on the local stack so the
    -- parser can replace the parameters while parsing 
    push_vars_to_stack vars target_vars = 
        if length vars /= length target_vars
          then pzero
          else mapM_ (updateState . push_to_stack) 
                     (zip target_vars vars)

               
    



