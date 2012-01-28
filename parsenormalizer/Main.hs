import Lambda hiding (main)
import System
import System.Environment
import System.IO
import Directory
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Token     
import Text.ParserCombinators.Parsec.Language

lambdaDef = emptyDef{ identStart = letter
              , identLetter = alphaNum <|> oneOf "_'"
              , opStart = oneOf ""
              , opLetter = oneOf ""
              , reservedOpNames = ["\\", ".", "->", " = "]
              , reservedNames = ["let", "in"]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reserved = m_reserved
           , reservedOp = m_reservedOp
           , whiteSpace = m_whiteSpace } = makeTokenParser lambdaDef

appParser :: Parser Term
appParser = termParser `chainl1` (return App)

absParser :: Parser Term
absParser = do
    {  var <- m_identifier
    ;  (m_reservedOp ".") <|> (m_reserved "->")
    ;  term <- termParser
    ;  return $ Abs var term
    }
    <|> do
    {  var <- m_identifier
    ;  term <- absParser
    ;  return $ Abs var term  
    }

termParser :: Parser Term
termParser = do
    {  m_parens termParser >>= return
    }
    <|> do
    {  m_reservedOp "\\"
    ;  absParser >>= return
    }
    <|> do
    {  v <- m_identifier
    ;  return $ Var v
    }

fileParser :: Parser Term
fileParser = do 
     {  m_reserved "let"
     ;  var <- m_identifier
     ;  m_reservedOp "="
     ;  what <- termParser
     ;  m_reserved "in"
     ;  term <- termParser
     ;  return $ subst var what term
     }
     <|> do
     {  term <- termParser
     ;  return term
     }
     <|> do
     {  x <- eof
     ;  return EmptyTerm
     }

main = do
  [fileToParse] <- getArgs
  exist <- doesFileExist fileToParse
  if exist
    then do
      content <- readFile fileToParse
      case (parse (m_whiteSpace >> fileParser) "" content) of
         Left err -> do {
                   putStr "parse error at";
                   print err
		} 
         Right x -> print {-$ normal'-} x
    else print "No such file in directory"
