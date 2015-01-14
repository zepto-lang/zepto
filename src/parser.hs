module Parser(readExpr, readExprList) where
import Types
import Numeric
import Control.Monad
import System.Environment
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf"\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do num <- parseDigital1 
                    <|> parseDigital2 
                    <|> parseHex 
                    <|> parseOct 
                    <|> parseBin
                 return $ num

parseDigital1 :: Parser LispVal
parseDigital1 = do x <- many1 digit 
                   (return . Number . read) x

parseDigital2 :: Parser LispVal
parseDigital2 = do try $ string "#d" 
                   x <- many1 digit 
                   (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig = bin2digx 0
bin2digx digint "" = digint
bin2digx digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in 
                            bin2digx old xs

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseLet :: Parser LispVal
parseLet = do string "let"
              return $ Atom "define"

parseBool = do string "#"
               x <- oneOf "tf"
               return $ case x of
                        't' -> Bool True
                        'f' -> Bool False

parseChar :: Parser LispVal
parseChar = do try $ string "#\\"
               x <- parseCharName <|> anyChar
               return $ Character x

parseCharName = do x <- try (string "space" <|> string "newline")
                   case x of
                    "space" -> do return ' '
                    "newline" -> do return '\n'

parseExpr :: Parser LispVal
parseExpr = try parseLet
        <|> parseAtom
        <|> parseString
        <|> parseQuoted
        <|> try parseNumber
        <|> try parseBool
        <|> try parseChar
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)
