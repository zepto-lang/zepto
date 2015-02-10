module Parser(readExpr, readExprList) where
import Types
import Numeric
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do _ <- char '"'
                 x <- many (noneOf"\"")
                 _ <- char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first : rest
               return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = try parseReal
              <|> parseDigital1
              <|> parseDigital2 
              <|> parseHex 
              <|> parseOct 
              <|> parseBin

parseReal :: Parser LispVal
parseReal = do neg <- optionMaybe $ string "-"
               before <- many1 digit
               _ <- string "."
               after <- many1 digit
               case neg of
                    Just _ -> (return . Number . NumF . read) ("-" ++ before ++ "." ++ after)
                    Nothing -> (return . Number . NumF . read) (before ++ "." ++ after)

parseDigital1 :: Parser LispVal
parseDigital1 = do neg <- optionMaybe $ string "-"
                   x <- many1 digit
                   case neg of
                      Just _ -> (return . Number . NumI . read) ("-" ++ x)
                      Nothing -> (return . Number . NumI . read) x

parseDigital2 :: Parser LispVal
parseDigital2 = do _ <- try $ string "#d" 
                   x <- many1 digit 
                   (return . Number . NumI . read) x

parseHex :: Parser LispVal
parseHex = do _ <- try $ string "#x"
              x <- many1 hexDigit
              return $ Number $ NumI (hex2dig x)

parseOct :: Parser LispVal
parseOct = do _ <- try $ string "#o"
              x <- many1 octDigit
              return $ Number $ NumI (oct2dig x)

parseBin :: Parser LispVal
parseBin = do _ <- try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number $ NumI (bin2dig x)

oct2dig :: String -> Integer
oct2dig x = fst $ head $ readOct x

hex2dig :: String -> Integer
hex2dig x = fst $ head $ readHex x

bin2dig :: String -> Integer
bin2dig = bin2digx 0

bin2digx :: Integer -> String -> Integer
bin2digx digint "" = digint

bin2digx digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in 
                            bin2digx old xs

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do h <- endBy parseExpr spaces
                     t <- char '.' >> spaces >> parseExpr
                     return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do _ <- char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseLet :: Parser LispVal
parseLet = do _ <- string "let"
              return $ Atom "define"

parseBool :: Parser LispVal
parseBool = do _ <- string "#"
               x <- oneOf "tf"
               return $ case x of
                        't' -> Bool True
                        'f' -> Bool False
                        _   -> error "This will never happen."

parseChar :: Parser LispVal
parseChar = do _ <- try $ string "#\\"
               x <- parseCharName <|> anyChar
               return $ Character x

parseCharName :: Parser Char
parseCharName = do x <- try (string "space" <|> string "newline")
                   case x of
                    "space"   -> return ' '
                    "newline" -> return '\n'
                    _         -> return '\0'

parseComments :: Parser (ParseError -> LispError)
parseComments = string ";" >> manyTill anyChar newline >> return Parser

parseExpr :: Parser LispVal
parseExpr = do optional $ many1 parseComments
               try parseLet <|> 
                try parseNumber <|> 
                 parseAtom <|> 
                  parseString  <|> 
                   parseQuoted <|> 
                    try parseBool <|> 
                     try parseChar <|> 
                     do _ <- char '('
                        x <- try parseList <|> parseDottedList
                        _ <- char ')'
                        return x

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser input input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)
