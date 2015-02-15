module Parser(readExpr, readExprList) where
import Types
import Numeric
import Data.Char
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
parseAtom = do first <- letter <|> symbol <|> oneOf "."
               rest <- many (letter <|> digit <|> symbol <|> oneOf ".")
               let atom = first : rest
               if atom == "."
                   then pzero
                   else return $ Atom atom

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

parseBool :: Parser LispVal
parseBool = do _ <- string "#"
               x <- oneOf "tf"
               return $ case x of
                        't' -> Bool True
                        'f' -> Bool False
                        _   -> error "This will never happen."

parseChar :: Parser LispVal
parseChar = do
  _ <- try (string "#\\")
  c <- anyChar
  r <- many (letter <|> digit)
  let pchr = c : r
  case pchr of
    "alarm"     -> return $ Character '\a' 
    "backspace" -> return $ Character '\b' 
    "delete"    -> return $ Character '\DEL'
    "escape"    -> return $ Character '\ESC' 
    "newline"   -> return $ Character '\n'
    "null"      -> return $ Character '\0' 
    "return"    -> return $ Character '\n' 
    "space"     -> return $ Character ' '
    "tab"       -> return $ Character '\t'
    _ -> case c : r of
        [ch] -> return $ Character ch
        ('x' : hexs) -> do
            rv <- parseHexScalar hexs
            return $ Character rv
        _ -> pzero

parseHexScalar :: Monad m => String -> m Char
parseHexScalar num = do
    let ns = Numeric.readHex num
    case ns of
        [] -> fail $ "Unable to parse hex value " ++ show num
        _ -> return $ chr $ fst $ head ns

parseComments :: Parser (ParseError -> LispError)
parseComments = (string ";" <|> many1 newline) >> manyTill anyChar newline >> return ParseErr

parseExpr :: Parser LispVal
parseExpr = do optional $ many1 parseComments
               try parseNumber <|> 
                 try parseAtom <|> 
                  parseString  <|> 
                   parseQuoted <|> 
                    try parseBool <|> 
                     try parseChar <|> 
                     do _ <- char '('
                        x <- try parseList <|> parseDottedList
                        _ <- char ')'
                        return x
                    <?> "Expression"

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser input input of
    Left err -> throwError $ ParseErr err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)
