module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "repeat", "skip", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )
----------------------------------
--- Parser de expressiones enteras
-----------------------------------
-- Parser de los símbolos de términos. ('+' y '-')
parseTermSymbol = (do reservedOp lis "+"
                      return (Plus)
                    <|> (do reservedOp lis "-"
                            return (Minus)))

-- Parser de los símbolos de factores. ('*' y '/')
parseFactSymbol = (do reservedOp lis "*"
                      return (Times)
                     <|> (do reservedOp lis "/"
                             return (Div)))

parseCommaSymbol = (do reservedOp lis ","
                       return (ESeq))

-- Parser de números naturales
parseNat ::Parser (Exp Int)
parseNat = (do n <- (natural lis)
               return (Const (fromIntegral n)))

-- Parser de nombres de variables
parseVar ::Parser (Exp Int)
parseVar = (do x <- (identifier lis)
               return (Var x))

-- Parser de números enteros negativos
parseUMin :: Parser (Exp Int)
parseUMin = (do reservedOp lis "-"
                n <- (try parseNat) <|> parseIntExpression
                return (UMinus n))
              
-- Parser de expresiones enteras
intexp :: Parser (Exp Int)
intexp = chainr1 parseAssExpr parseCommaSymbol

parseAssExpr :: Parser (Exp Int)
parseAssExpr = try (do {v <- identifier lis;
                        reservedOp lis "=";
                        e <- parseAssExpr;
                        return (EAssgn v e)})
               <|> do { e <- parseIntExpression; return e }

-- Parser de expresiones enteras sin tener en cuenta asignación y operador ,
parseIntExpression :: Parser (Exp Int)
parseIntExpression = chainl1 parseTerm parseTermSymbol

-- Parser de términos
parseTerm :: Parser (Exp Int)
parseTerm = chainl1 parseFact parseFactSymbol

-- Parser de factores
parseFact :: Parser (Exp Int)
parseFact = (try parseUMin)
            <|> (parseVar)
            <|> (try parseNat)
            <|> (try (parens lis intexp))


-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

-- Parser de valores booleanos
parseBoolVal :: Parser (Exp Bool)
parseBoolVal = do { reserved lis "true"; return BTrue }
               <|> do { reserved lis "false"; return BFalse }

-- Parser de operadores de comparación
parseComparisonOp = do { reservedOp lis "==" ; return (Eq) }
                    <|> do { reservedOp lis "!=" ; return (NEq) }
                    <|> do { reservedOp lis "<" ; return (Lt) }
                    <|> do { reservedOp lis ">" ; return (Gt) }


-- Parser de comparaciones
parseComparison :: Parser (Exp Bool)
parseComparison = (do x <- intexp
                      comp <- parseComparisonOp
                      y <- intexp
                      return (comp x y))

-- Parser de operador not
parseNot :: Parser (Exp Bool)
parseNot = (do reservedOp lis "!"
               x <- boolexp
               return (Not x))

-- Parser de expresiones booleanas
-- Siguiendo el orden de precedencia dado en el enunciado, tenemos en cuenta primero las expresiones OR y luego pasamos a las expresiones AND
boolexp :: Parser (Exp Bool)
boolexp = chainl1 parseIntoOr (do { reservedOp lis "||" ; return (Or) })


-- Parser de términos
parseIntoOr :: Parser (Exp Bool)
parseIntoOr = chainl1 parseIntoAnd (do { reservedOp lis "&&"; return (And) })

-- Parser de factores
parseIntoAnd :: Parser (Exp Bool)
parseIntoAnd = (try parseBoolVal)
               <|> (try parseNot)
               <|> (try parseComparison)
               <|> (parens lis boolexp)

-----------------------------------
--- Parser de comandos
-----------------------------------

-- Parser de comando 'Skip'
parseSkip :: Parser Comm
parseSkip = (do (reserved lis "skip")
                return Skip)

-- Parser de operador Let (???)
parseLet :: Parser Comm
parseLet = (do v <- identifier lis
               reservedOp lis "="
               e <- intexp
               return (Let v e))

-- Parser de comando de secuencia
parseSeq = do { reservedOp lis ";"; return (Seq) }

-- Parser de ejecución condicional
parseIfThenElse :: Parser Comm
parseIfThenElse = (do (reserved lis "if")
                      cond <- boolexp
                      (reservedOp lis "{")
                      commT <- comm
                      (reservedOp lis "}")
                      (reserved lis "else")
                      (reservedOp lis "{")
                      commF <- comm
                      (reservedOp lis "}")
                      return (IfThenElse cond commT commF))

-- Parser de ejecución condicional sin rama else
parseIfThen :: Parser Comm
parseIfThen = (do (reserved lis "if")
                  cond <- boolexp
                  (reservedOp lis "{")
                  commT <- comm
                  (reservedOp lis "}")
                  return (IfThen cond commT))

-- Parser de bucle
parseRep ::  Parser Comm
parseRep = (do  reserved lis "repeat"
                c <- comm
                reserved lis "until"
                cond <- boolexp
                reserved lis "end"
                return (Repeat c cond))

-- Parser de comandos
comm1 :: Parser Comm
comm1 = (try parseSkip)
        <|> (try parseLet)
        <|> (try parseIfThenElse)
        <|> (try parseIfThen)
        <|> (try parseRep)

comm :: Parser Comm
comm = chainl1 comm1 parseSeq

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
