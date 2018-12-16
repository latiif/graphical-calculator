module Expr where
import Parsing
import Data.Maybe
import Data.List.Split
import Text.Printf
import Test.QuickCheck

-- Define the data type for our expression
data Expr =  Number Double
           | Add Expr Expr
           | Mult Expr Expr
           | Cosine Expr
           | Sine Expr
           | Variable
        deriving Eq


        
-- We want to define our own implementation of Show        
instance Show Expr where
  show = showExpr



-- Used to display paranthesis in a way that preserves information  
showExpr :: Expr -> String
showExpr (Number x) = printf "%f" x 

showExpr (Add op1@(Number _) op2@(Number _))   = showExpr op1 ++ "+" ++ showExpr op2  
showExpr (Add op1 op2)                         = showExpr op1 ++ "+" ++ showFactor op2  

showExpr (Mult op1@(Number _) op2@(Number _))  = showExpr op1 ++ "*" ++ showExpr op2  
showExpr (Mult op1 op2)                        = showExpr op1 ++ "*" ++ showFactor op2 


showExpr (Variable)                            = "x"  
showExpr (Cosine op1)                          = "cos(" ++ showExpr op1 ++ ")"
showExpr (Sine op1)                            = "sin(" ++ showExpr op1++ ")"

-- Borrowed form the lecture notes with adapatations
showFactor :: Expr -> String
showFactor (Add a b)                           = "("++ showExpr (Add a b) ++")"
showFactor (Mult a b)                          = "("++ showExpr (Mult a b) ++")"
showFactor e = showExpr e


-- Evaulates the expression given a value for the variable
eval :: Expr -> Double -> Double
eval (Number    x)   _   = x
eval (Add  op1 op2) val  = eval op1 val + eval op2 val
eval (Mult op1 op2) val  = eval op1 val * eval op2 val
eval  Variable val       = val
eval (Cosine   op1) val  = cos $ eval op1 val
eval (Sine     op1) val  = sin $ eval op1 val

-- Parsing
-- | Parse a number, float or integer (adapted with changes from lecture notes)
number :: Parser Double
number = floating <|> 
        (read <$> oneOrMore digit) 

-- Borrowed from lecture notes and example in the module Parsing
digits :: Parser String
digits =  oneOrMore digit

-- Given "x.y" returns the double value of them
merge :: String -> String -> Double
merge x y = read (x ++"."++y) :: Double

-- | Parse a specific sequence of characters - borrwoed directly form lecture notes
string :: String -> Parser String
string ""    = return ""
string (c:s) = do c' <- char c
                  s' <- string s
                  return (c':s')

-- Parser for decimal point numbers
floating :: Parser Double
floating = do int <- digits
              char '.'
              dec <- digits
              return (merge int dec)

-- Addition, multiplication and operator are borrowed form lecture notes              
-- | Parse two numbers, separated by +, and add them
addition :: Parser Double
addition = operator '+' (+)

-- | Parse two numbers, separated by *, and multiply them
multiplication :: Parser Double
multiplication = operator '*' (*)

operator c op = do n1 <- number
                   char c
                   n2 <- number
                   return (n1 `op` n2)

-- |Generalizes out the process of parsing a unary operation                   
unaryFunction :: String -> (Expr -> Expr) -> Parser Expr
unaryFunction str unaryF = (
    (unaryF <$> (string (str++" ") *> term ))
    <|>
    (unaryF <$> (string (str++"(") *> expr <* char ')')))

-- Detects the existence and parses a variable
variable :: Parser Expr
variable = do 
            char 'x'
            return Variable

expr, term, factor :: Parser Expr
expr = leftAssoc Add  term (char '+')   -- Expression is built from adding on terms
term = leftAssoc Mult factor (char '*') -- Term is factors that are multiplied with other factors
factor =
    (char '(' *> expr <* char ')')      -- anything withint paranthesis (2+4)
    <|>
    (unaryFunction "cos" Cosine)        -- for cos
    <|>
    (unaryFunction "sin" Sine)          -- sin
    <|>
    variable                            -- variable (x)
    <|>
    (Number <$> floating)               -- floating number
    <|>
    (Number <$> number)                 -- an integer number
        
        
            
-- Borrowed from lecture,
-- Is used to aggregate the construction of a binary operation from left
leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
leftAssoc op item sep = do is <- chain item sep
                           return (foldl1 op is)

-- Given a string it tries to parse it into an expression
readExpr :: String -> Maybe Expr
readExpr s = case parse expr s of
             Just (e,"") -> return e
             _           -> Nothing
           where noSpaces = filter (/= ' ') s
 
           
-- Makes sure that parsing and reading an expression conserves information           
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e | translated == e = True
                    | otherwise = error $ show translated ++"\t"++ showExpr translated ++"\n" ++ show e ++"\t"++ showExpr e
    where translated = case (readExpr . show) e of
                        Just(e) -> e
                        _       -> error "Unable to read expression" 

-- We need to be able to create random instances of our Expr type
instance Arbitrary Expr where
    arbitrary = sized arbExpr

arbExpr :: Int ->  Gen Expr
arbExpr n | n <=0  = do return $ Number 1
arbExpr 1 = oneof [do return Variable,
	           do k <- arbitrary
		      return $ (Number $  abs(k))]
arbExpr n | even n = do 
		      op1 <- arbExpr ((n-1) `div` 2)
		      op2 <- arbExpr ((n-1) `div` 2)
	              oneof [do return $ Add op1 op2,
			     do return $  Mult op1 op2]			
arbExpr n | odd n  = do 
		      op1 <- arbExpr (n-1)
		      oneof [do return $ Cosine op1,
			     do return $ Sine op1]		



-- Simplifies an expression
simplify :: Expr -> Expr
simplify exp = if sizeExpr simplified < sizeExpr exp
                then simplify simplified
                else exp
        where simplified = simplify' exp

-- called by the simplify wrapper, open for additional rules        
simplify' :: Expr -> Expr
simplify' (Add exp (Number 0)) = simplify' exp
simplify' (Add (Number 0) exp) = simplify' exp

simplify' (Mult exp (Number 1)) = simplify' exp
simplify' (Mult (Number 1) exp) = simplify' exp

simplify' (Mult exp (Number 0)) = Number 0
simplify' (Mult (Number 0) exp) = Number 0

simplify' (Add (Number n1) (Number n2)) = Number $ n1 + n2
simplify' (Mult (Number n1) (Number n2)) = Number $ n1 * n2
simplify' (Cosine (Number n)) = Number $ cos n
simplify' (Sine (Number n)) = Number $ sin n

simplify' Variable = Variable
simplify' (Add e1 e2) = Add (simplify' e1) (simplify' e2)
simplify' (Mult e1 e2) = Mult (simplify' e1) (simplify' e2)
simplify' (Cosine e) = Cosine (simplify' e)
simplify' (Sine e) = Sine (simplify' e)
simplify' e = e


-- Calculates the size of an expression
sizeExpr :: Expr -> Int
sizeExpr (Number _)     = 1
sizeExpr Variable       = 1
sizeExpr (Add op1 op2)  = 1 + sizeExpr op1 + sizeExpr op2
sizeExpr (Mult op1 op2) = 1 + sizeExpr op1 + sizeExpr op2
sizeExpr (Cosine op)    = 1 + sizeExpr op
sizeExpr (Sine op)      = 1 + sizeExpr op 

-- Makes sure that the size of the simplified expression is <= than the original
-- Also runs some eval checks ot makes sure they yield the same values form a random range
prop_simplify :: Expr -> Double -> Bool
prop_simplify exp val = equivalent && sizeChange
                where simplified = simplify exp
                      equivalent = all (== True)  [eval simplified x == eval exp x | x<- [(-val')..val']]
                      sizeChange = sizeExpr simplified <= sizeExpr exp
                      val' = abs val

-- Wrapper function for differentiation and it simplifies
differentiate :: Expr -> Expr
differentiate = simplify . differentiate'

-- Just some plain old diff rules
differentiate' :: Expr -> Expr
differentiate' (Number _) = Number 0
differentiate' Variable   = Number 1
differentiate' (Add op1 op2) = Add (differentiate' op1) (differentiate' op2)
differentiate' (Mult op1 op2) = Add (Mult (differentiate' op1) op2) (Mult (differentiate' op2) op1)
differentiate' (Cosine op) = Mult (differentiate' op) (Mult (Number (-1)) (Sine op))
differentiate' (Sine op) = Mult (differentiate' op) (Cosine op)
