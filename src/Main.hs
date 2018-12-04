import Expr
import Parsing
import Control.Monad(forever)

main :: IO ()
main = forever readEvalPrint

readEvalPrint = do
    s <- getLine
    case parse expr s of
        Just (e,"") -> putStrLn . show $ eval e 1
        _           -> putStrLn "error"