module Expr where
import Parsing
import Data.Maybe
import Data.List.Split
import Text.Printf
import Test.QuickCheck

data Expr =               Number Float
                     |    Add Expr Expr
                     |    Mult Expr Expr
                     |    Cosine Expr
                     |    Sine Expr
                     |    Variable
            deriving (Eq,Show)


            {-
instance Show Expr where
  show = showExpr
-}

  {-
instance Eq Expr where
    e1 == e2 = bruteForceEval e1 e2
    -}

bruteForceEval :: Expr -> Expr -> Bool
bruteForceEval e1 e2  = eval1 == eval2 
    where eval1 = simplify e1
          eval2 = simplify e2

showExpr :: Expr -> String
showExpr (Number x) = printf "%f" x 

showExpr (Add op1@(Number _) op2@(Number _))    = showExpr op1 ++ "+" ++ showExpr op2  
showExpr (Add op1 op2)                           = showExpr op1 ++ "+" ++ showFactor op2  
--showExpr (Add op1 op2)                          = showExpr op1 ++ "+" ++ showExpr op2

showExpr (Mult op1@(Number _) op2@(Number _))    = showExpr op1 ++ "*" ++ showExpr op2  
showExpr (Mult op1 op2)                           = showExpr op1 ++ "*" ++ showFactor op2 


showExpr (Variable) = "x"  
--showExpr (Mult op1 op2) = showFactor op1 ++ "*" ++ showFactor op2
showExpr (Cosine op1) = "cos(" ++ showExpr op1 ++ ")"
showExpr (Sine op1) = "sin(" ++ showExpr op1++ ")"

showFactor :: Expr -> String
showFactor (Add a b) = "("++ showExpr (Add a b) ++")"
showFactor (Mult a b) = "("++ showExpr (Mult a b) ++")"

showFactor e = showExpr e




eval :: Expr -> Float -> Float
eval (Number x) _ = x
eval (Add op1 op2) val = eval op1 val + eval op2 val
eval (Mult op1 op2) val = eval op1 val * eval op2 val
eval Variable val = val
eval (Cosine op1) val = cos $ eval op1 val
eval (Sine op1) val = sin $ eval op1 val

-- Parsing
-- | Parse a number, integer or decimal
number :: Parser Float
number = floating <|> (read <$> oneOrMore digit) 


digits :: Parser String
digits =  oneOrMore digit

-- Given "x.y" returns the float value of them
merge :: String -> String -> Float
merge x y = read (x ++"."++y) :: Float

-- | Parse a specific sequence of characters
string :: String -> Parser String
string ""    = return ""
string (c:s) = do c' <- char c
                  s' <- string s
                  return (c':s')

-- Parser for decimal point numbers
floating :: Parser Float
floating = do int <- digits
              char '.'
              dec <- digits
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
prop_ShowReadExpr e | translated == e = True
                    | otherwise = error $ show translated ++"\t"++ showExpr translated ++"\n" ++ show e ++"\t"++ showExpr e
    where translated = fromJust $ (readExpr . showExpr) e                    




instance Arbitrary Expr where
    arbitrary = sized arbExpr

arbExpr :: Int ->  Gen Expr
arbExpr n | n <=0  = do return $ Number 1
arbExpr 1 = oneof [do return Variable,
	           do k <- arbitrary
		      return $ (Number $  abs(k))]
arbExpr n | even n = do 
		      op1 <- arbExpr (n-1)
		      op2 <- arbExpr (n-2)
	              oneof [do return $ Add op1 op2,
			     do return $  Mult op1 op2]			
arbExpr n | odd n  = do 
		      op1 <- arbExpr (n-1)
		      oneof [do return $ Cosine op1,
			     do return $ Sine op1]		



simplify :: Expr -> Expr
simplify exp = if sizeExpr simplified < sizeExpr exp
                then simplify simplified
                else exp
        where simplified = simplify' exp

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



sizeExpr :: Expr -> Int
sizeExpr (Number _)     = 1
sizeExpr Variable       = 1
sizeExpr (Add op1 op2)  = sizeExpr op1 + sizeExpr op2
sizeExpr (Mult op1 op2) = sizeExpr op1 + sizeExpr op2
sizeExpr (Cosine op)    = sizeExpr op
sizeExpr (Sine op)      = sizeExpr op 


prop_simplify :: Expr -> Float -> Bool
prop_simplify exp val = equivalent && sizeChange
                where simplified = simplify exp
                      equivalent = all (== True)  [eval simplified x == eval exp x | x<- [(-val')..val']]
                      sizeChange = sizeExpr simplified <= sizeExpr exp
                      val' = abs val


differentiate :: Expr -> Expr
differentiate = simplify . differentiate'


differentiate' :: Expr -> Expr
differentiate' (Number _) = Number 0
differentiate' Variable   = Number 1
differentiate' (Add op1 op2) = Add (differentiate' op1) (differentiate' op2)
differentiate' (Mult op1 op2) = Add (Mult (differentiate' op1) op2) (Mult (differentiate' op2) op1)
differentiate' (Cosine op) = Mult (differentiate' op) (Mult (Number (-1)) (Sine op))
differentiate' (Sine op) = Mult (differentiate' op) (Cosine op)




