%include polycode.fmt
\section{Parser}

\noindent
Parses a file containing expressions of the TLBN language.  Adapted from TAPL
fullsimple parser code.

\begin{code}
module Parser (parseTLBN) where

import TLBNError
import TLBN
import Context
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Prim (ParsecT)
import Control.Monad
import Data.Functor.Identity (Identity)
import Data.Char (isUpper)
\end{code}

\noindent
Helper routine that will either fail on error or return the value associated
with the action.
\begin{code}
throwsToParser :: (Show a1, Monad m) => Either a1 a -> m a
throwsToParser action =
    case action of
      Left err  -> fail $ show err
      Right val -> return val

tlbnLangDef :: GenLanguageDef String u Identity
tlbnLangDef = LanguageDef
              { commentStart    = "/*"
              , commentEnd      = "*/"
              , commentLine     = ""
              , nestedComments  = False
              , identStart      = letter
              , identLetter     = letter <|> digit
              , opStart         = fail "no operators"
              , opLetter        = fail "no operators"
              , reservedOpNames = []
              , caseSensitive   = True
              , reservedNames   = ["true",
                                   "false",
                                   "if", "then", "else", "fi",
                                   "Bool",
                                   "Nat",
                                   "succ",
                                   "pred",
                                   "iszero",
                                   "app",
                                   "abs",
                                   "fix"
                                  ]
              }

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser tlbnLangDef
-- Lexer tokens.
parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = P.parens lexer

identifier :: ParsecT String u Identity String
identifier = P.identifier lexer

reserved :: String -> ParsecT String u Identity ()
reserved = P.reserved lexer

symbol :: String -> ParsecT String u Identity String
symbol = P.symbol lexer

whiteSpace :: ParsecT String u Identity ()
whiteSpace = P.whiteSpace lexer

comma :: ParsecT String u Identity String
comma = P.comma lexer

colon :: ParsecT String u Identity String
colon = P.colon lexer

-- Type Parsing
parseTypeBool :: ParsecT String u Identity Type
parseTypeBool = reserved "Bool" >> return TyBool

parseTypeNat :: ParsecT String u Identity Type
parseTypeNat = reserved "Nat" >> return TyNat

-- Parse (Type -> Type) types.
parseTypeArr :: ParsecT String u Identity Type
parseTypeArr = parseTypeBool <|>
               parseTypeNat <|>
               parens parseType

-- Parse types.
parseType :: ParsecT String u Identity Type
parseType = parseTypeArr `chainr1` (symbol "->" >> return TyArr)

-- Zero argument term parsing
parseTrue :: ParsecT String u Identity Term
parseTrue  = reserved "true"  >> return TrmTru

parseFalse :: ParsecT String u Identity Term
parseFalse = reserved "false" >> return TrmFls

parseZero :: ParsecT String u Identity Term
parseZero = symbol "0" >> return TrmZero

-- Helper routines that is used in one arg parses.
parseOneArg :: String -> (Term -> b) -> ParsecT String Context Identity b
parseOneArg keyword constructor = reserved keyword >>
                                  liftM constructor parseTerm

parseSucc :: ParsecT String Context Identity Term
parseSucc = parseOneArg "succ" TrmSucc

parsePred :: ParsecT String Context Identity Term
parsePred = parseOneArg "pred" TrmPred

parseIsZero :: ParsecT String Context Identity Term
parseIsZero = parseOneArg "iszero" TrmIsZero

-- Parses syntactic form for general recursion, fix
parseFix :: ParsecT String Context Identity Term
parseFix = do
    reserved "fix"
    t <- parseTerm
    return (TrmFix t)

-- Parses ifs of the form: If t then t else t fi.
parseIf :: ParsecT String Context Identity Term
parseIf = do
    reserved "if"
    t1 <- parseTerm
    reserved "then"
    t2 <- parseTerm
    reserved "else"
    t3 <- parseTerm
    reserved "fi"
    return (TrmIf t1 t2 t3)

-- Parses variables and provides it a variable ID for later use.
parseVar :: ParsecT String Context Identity Term
parseVar = do
    varName <- identifier
    if isUpper $ head varName
      then fail "variables must start with a lowercase letter"
      else do
           context <- getState
           idx <- throwsToParser $ indexOf varName context
           return $ TrmVar idx (ctxLength context)

-- Parses var : Type terms.
parseVarBind :: ParsecT String Context Identity Term
parseVarBind = do
    var <- identifier
    _ <- colon
    ty <- parseType
    let binding = VarBind ty
    updateState $ appendBinding var binding
    return (TrmBind var binding)

-- Parses abstractions of the form:
-- abs (var:Type . body)
parseAbs :: ParsecT String Context Identity Term
parseAbs = do
    reserved "abs"
    _ <- symbol "("
    context <- getState
    (TrmBind varStr (VarBind typ)) <- parseVarBind
    _ <- symbol "."
    body <- parseTerm
    _ <- symbol ")"
    setState context
    return (TrmAbs varStr typ body)

-- Implements app parsing of the form app (t1, t2).
parseApp :: ParsecT String Context Identity Term
parseApp = do
    reserved "app"
    _ <- symbol "("
    trmFn <- parseTerm
    _ <- comma
    trmArg <- parseTerm
    _ <- symbol ")"
    return (TrmApp trmFn trmArg)

-- Puts all our parsers together.
parseTerm :: ParsecT String Context Identity Term
parseTerm = parseTrue <|>
            parseFalse <|>
            parseSucc <|>
            parsePred <|>
            try parseIsZero <|> -- try because of if
            parseIf <|>
            parseFix <|>
            parseZero <|>
            parseApp <|>
            parseAbs <|>
            parseVar <|>
            parens parseTerm

-- Top-level function that parseTLBN interacts with.
parseTerms :: ParsecT String Context Identity Term
parseTerms = do
    whiteSpace -- lexer handles whitespace everywhere except here
    ts <- parseTerm
    eof
    return ts
\end{code}

\noindent
Top-level routine that is responsible for parsing the provided source text.
Terms will be returned or an error will be raised if the parse fails. Sets up
new empty context.
\begin{code}
parseTLBN :: String -> ThrowsError Term
parseTLBN srcStr =
    case runParser parseTerms newContext "TLBN Parser" srcStr of
      Left err   -> error (show err)
      Right term -> return term
\end{code}
