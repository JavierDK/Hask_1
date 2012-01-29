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

varParser :: Parser Term
varParser = do
  {  var <- m_identifier;
  ;  return $ Var var
  }

absParser :: Parser Term
absParser = do
   { var <- m_identifier
   ;do
     {  (m_reservedOp ".") <|> (m_reserved "->")
     ;  term <- termParser
     ;  return $ Abs var term
     }
     <|> do
     {  term <- absParser
     ;  return $ Abs var term  
     }
   }

termParser :: Parser Term
termParser = do
    {  m_parens appParser >>= return
    }
    <|> do
    {  m_reservedOp "\\"
    ;  absParser >>= return
    }
    <|> do
    {  varParser >>= return
    }

fileParser :: Parser Term
fileParser = do 
     {  m_reserved "let"
     ;  var <- m_identifier
     ;  m_reservedOp "="
     ;  what <- appParser
     ;  m_reserved "in"
     ;  term <- fileParser
     ;  return $ subst var what term
     }
     <|> do
     {  term <- appParser
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
         Right x -> print $ normal' x
    else print "No such file in directory"
