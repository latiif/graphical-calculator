-- | Parsing
-- Examples to illustrate how to write parsers using parsing combinators
-- Functional Programming course 2018.
-- Thomas Hallgren

{-
This started as a skeleton, the definitions were filled in
during the lecture.
-}
module ParsingExamples where
    import Data.Char(isDigit)
    import Parsing hiding (chain,digit)
    import Control.Monad(forever)
    
    --------------------------------------------------------------------------------
    -- * A first example
    -- Writing a recursive decent parser directly
    -- Using functions of type String -> Maybe (a,String)
    
    {- BNF:
    digit = "0".."9".
    number = digit{digit}.
    addition = number "+" number.
    -}
    
    number_v1 :: String -> Maybe (Integer,String)
    number_v1 s = case span isDigit s of
                    ("",_) -> Nothing
                    (ds,r) -> Just (read ds,r)
    
    addition_v1 :: String -> Maybe (Integer,String)
    addition_v1 s = case number_v1 s of
                      Just (n1,'+':r1) -> case number_v1 r1 of
                                            Just (n2,r2) -> Just (n1+n2,r2)
                                            _ -> Nothing
                      _ -> Nothing
    
    
    {- A small extension to the BNF
    multiplication ::= number "*" number.
    calculation    ::= addition | multiplication.
    -}
    
    multiplication_v1 :: String -> Maybe (Integer,String)
    multiplication_v1 s = case number_v1 s of
                            Just (n1,'*':r1) -> case number_v1 r1 of
                                                  Just (n2,r2) -> Just (n1*n2,r2)
                                                  _ -> Nothing
                            _ -> Nothing
    
    
    calculation_v1 :: String -> Maybe (Integer,String)
    calculation_v1 s = case addition_v1 s of
                         Nothing -> multiplication_v1 s
                         result  -> result
                     
    
    --------------------------------------------------------------------------------
    -- * Rewriting our first example using parsing combinators
    
    -- | Parse a digit (also available in the Parsing module)
    digit :: Parser Char
    digit = sat isDigit
    
    -- | Parse a number
    number :: Parser Double
    number = (read <$> oneOrMore digit)
          <|> floating
            
        
             
             --do ds <- oneOrMore digit
             --   return (read ds)

    numberLength :: Double -> Int
    numberLength x = (-2) + (length $ show x)

    merge :: Double -> Double -> Double
    merge x y = x + y/(10^numberLength(y))

    floating :: Parser Double
    floating = do int <- number
                  char '.'
                  dec <- number
                  return (merge int dec)
    
    -- | Parse two numbers, separated by +, and add them
    addition :: Parser Double
    {-
    addition = do n1 <- number
                  char '+'
                  n2 <- number
                  return (n1+n2)
    -}
    addition = operator '+' (+)
    
    -- | Parse two numbers, separated by *, and multiply them
    multiplication :: Parser Double
    {-
    multiplication =do n1 <- number
                       char '*'
                       n2 <- number
                       return (n1*n2)
    -}
    multiplication = operator '*' (*)
    
    operator c op = do n1 <- number
                       char c
                       n2 <- number
                       return (n1 `op` n2)
    
    
    calculation :: Parser Double
    calculation = addition <|> multiplication
    
    --------------------------------------------------------------------------------
    -- * An expression parser (version 1)
    
    data Expr = Num Double
              | Add Expr Expr
              | Mul Expr Expr
              | Sin Expr
              | Cos Expr
              | Var
              deriving (Eq,Show)
    
    eval :: Expr -> Double
    eval (Num n) = n * 1.0
    eval (Add a b) = eval a + eval b
    eval (Mul a b) = eval a * eval b
    eval (Sin a)  = sin $ eval a
    eval (Cos a)  = cos $ eval a
    eval (Var)  = 1
    
    {- EBNF:
    expr   ::= term {"+" term}.
    term   ::= factor {"*" factor}.
    factor ::= number | "(" expr ")".
    -}
    {-
    expr, term, factor :: Parser Expr
    
    expr = do t <- term
              ts <- zeroOrMore (do char '+'; term)
              return (foldl1 Add (t:ts))
    
    term = do f <- factor
              fs <- zeroOrMore (do char '*'; factor)
              return (foldl1 Mul (f:fs))
    
    factor = -- Num <$> number
             do n <- number
                return (Num n)
             <|>
             do char '('
                e <- expr
                char ')'
                return e
    -}
    --------------------------------------------------------------------------------
    -- * A more elegant expression parser
    
    
    expr, term, factor :: Parser Expr
    expr = leftAssoc Add term (char '+')
    term = leftAssoc Mul factor (char '*')
    factor =
            (char '(' *> expr <* char ')')
            <|>
            (
                (Cos <$> (string "cos " *> term ))
                <|>
                (Cos <$> (string "cos (" *> expr <* char ')'))
            )
            <|>
            (
                (Sin <$> (string "sin " *> term ))
                <|>
                (Sin <$> (string "sin (" *> expr <* char ')'))
            )
            <|>
            variable
            <|>
            (Num <$> floating) 
            <|>
            (Num <$> number)
            
            
            
            
            
             
    
     
             
    variable :: Parser Expr
    variable = (do 
                char 'x'
                return Var
                )
    
    -- | Parse a list of items with separators
    -- (also available in the Parsing module)
    chain :: Parser item -> Parser sep -> Parser [item]
    chain item sep = do i <- item
                        is <- zeroOrMore (do sep; item)
                        return (i:is)
    
    leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
    leftAssoc op item sep = do is <- chain item sep
                               return (foldl1 op is)
    
    rightAssoc op item sep = undefined -- exercise
    
    --------------------------------------------------------------------------------
    -- * The simple calculator example
    
    main = do putStrLn "Welcome to the simple calculator!"
              forever readEvalPrint
    
    readEvalPrint = do putStr "Expression? "
                       s <- getLine
                       case parse expr s of
                         Just (e,"") -> do putStr "Value: "
                                           print (eval e)
                         _ -> putStrLn "Syntax error!"
    
    
    
    
    
    
    
    
    
    
    
    
    
    --------------------------------------------------------------------------------
    -- * More examples
    
    -- ** Data types with infix operatos
    infixl 6 :+
    infixl 7 :*
    
    data Expr2 = C Integer
               | Expr2 :+ Expr2
               | Expr2 :* Expr2
               deriving (Show,Read)  -- gives us almost what we want
    
    ex1 = C 2
    ex2 = ex1 :+ ex1
    ex3 = C 1 :+ C 2 :* C 3
    ex4 = (C 1 :+ C 2) :* C 3
    
    
    -- | Parse a specific sequence of characters
    string :: String -> Parser String
    string ""    = return ""
    string (c:s) = do c' <- char c
                      s' <- string s
                      return (c':s')
    