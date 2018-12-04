module Expr where
import Parsing
import Data.Maybe
import Data.List.Split
import Test.QuickCheck

data Expr =     Number Float
                     |    Add Expr Expr
                     |    Mult Expr Expr
                     |    Cosine Expr
                     |    Sine Expr
                     |    Variable
            deriving Eq

instance Show Expr where
  show = showExpr

showExpr :: Expr -> String
showExpr (Number x) = show x 
showExpr (Add op1 op2) = showExpr op1 ++ "+" ++ showExpr op2  
showExpr (Variable) = "x"  
showExpr (Mult op1 op2) = showFactor op1 ++ "*" ++ showFactor op2
showExpr (Cosine op1) = "cos(" ++ showExpr op1 ++ ")"
showExpr (Sine op1) = "sin(" ++ showExpr op1++ ")"

showFactor :: Expr -> String
showFactor (Add a b) = "("++ showExpr (Add a b) ++")"
showFactor e = showExpr e



eval :: Expr -> Float -> Float
eval (Number x) _ = x
eval (Add op1 op2) val = (eval op1 val) + (eval op2 val)
eval (Mult op1 op2) val = (eval op1 val) * (eval op2 val)
eval (Variable) val = val
eval (Cosine op1) val = cos $ eval op1 val
eval (Sine op1) val = sin $ eval op1 val

-- Parsing
-- | Parse a number, integer or decimal
number :: Parser Float
number = (read <$> oneOrMore digit) <|> floating

-- Gives the number of digits in a number
numberLength :: Float -> Int
numberLength x = (-2) + (length $ show x)

-- Given "x.y" returns the double value of them
merge :: Float -> Float -> Float
merge x y = x + y/(10^numberLength(y))

-- | Parse a specific sequence of characters
string :: String -> Parser String
string ""    = return ""
string (c:s) = do c' <- char c
                  s' <- string s
                  return (c':s')

-- Parser for decimal point numbers
floating :: Parser Float
floating = do int <- number
              char '.'
              dec <- number
              return (merge int dec)

-- | Parse two numbers, separated by +, and add them
addition :: Parser Float
addition = operator '+' (+)

-- | Parse two numbers, separated by *, and multiply them
multiplication :: Parser Float
multiplication = operator '*' (*)

operator c op = do n1 <- number
                   char c
                   n2 <- number
                   return (n1 `op` n2)

unaryFunction :: String -> (Expr -> Expr) -> Parser Expr
unaryFunction str unaryF = (
    (unaryF <$> (string (str++" ") *> term ))
    <|>
    (unaryF <$> (string (str++"(") *> expr <* char ')')))

calculation :: Parser Float
calculation = addition <|> multiplication

expr, term, factor :: Parser Expr
expr = leftAssoc Add  term (char '+')
term = leftAssoc Mult factor (char '*')
factor =
    (char '(' *> expr <* char ')')
    <|>
    (unaryFunction "cos" Cosine)
    <|>
    (unaryFunction "sin" Sine)
    <|>
    variable
    <|>
    (Number <$> floating) 
    <|>
    (Number <$> number)
        
        
            
variable :: Parser Expr
variable = do 
            char 'x'
            return Variable
            

leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
leftAssoc op item sep = do is <- chain item sep
                           return (foldl1 op is)

readExpr :: String -> Maybe Expr
readExpr s = case parse expr s of
             Just (e,"") -> return e
             _           -> Nothing
           where noSpaces = filter (/= ' ') s
                   
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = translated == e
    where translated = fromJust $ (readExpr . show) e                    




instance Arbitrary Expr where
    arbitrary = sized arbExpr

arbExpr :: Int ->  Gen Expr
arbExpr n | n <=0  = do return $ Number 1
arbExpr 1 = oneof [do return Variable,
	           do k <- arbitrary
		      return $ (Number $ abs(k))]
arbExpr n | even n = do 
		      op1 <- arbExpr (n-1)
		      op2 <- arbExpr (n-2)
	              oneof [do return $ Add op1 op2,
			     do return $  Mult op1 op2]			
arbExpr n | odd n  = do 
		      op1 <- arbExpr (n-1)
		      oneof [do return $ Cosine op1,
			     do return $ Sine op1]		
