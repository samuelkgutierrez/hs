module Parser (parseTLBN) where

-- Adapted from TAPL fullsimple parser code.
-- Parses a file contain expressions of the TLBN language.

import TLBN
import Context
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Control.Monad
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity (Identity)
import Data.Char (isUpper)

throwsToParser :: (Show a1, Monad m) => Either a1 a -> m a
throwsToParser action =
    case action of
      Left err  -> fail $ show err
      Right val -> return val

fullSimpleDef :: GenLanguageDef String u Identity
fullSimpleDef = LanguageDef
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
                                     "abs"
                                    ]
                }

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser fullSimpleDef

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

natural :: ParsecT String u Identity Integer
natural = P.natural lexer

-- Type Parsing
parseTypeBool :: ParsecT String u Identity Type
parseTypeBool = reserved "Bool" >> return TyBool

parseTypeNat :: ParsecT String u Identity Type
parseTypeNat = reserved "Nat" >> return TyNat

parseTypeArr :: ParsecT String u Identity Type
parseTypeArr = parseTypeBool <|>
               parseTypeNat <|>
               parens parseType

parseType :: ParsecT String u Identity Type
parseType = parseTypeArr `chainr1` (symbol "->" >> return TyArr)

-- Zero argument term parsing
parseTrue :: ParsecT String u Identity Term
parseTrue  = reserved "true"  >> return TrmTru

parseFalse :: ParsecT String u Identity Term
parseFalse = reserved "false" >> return TrmFls

parseNat :: ParsecT String u Identity Term
parseNat = liftM numToSucc natural
    where numToSucc 0 = TrmZero
          numToSucc n = TrmSucc $ numToSucc (n - 1)

parseOneArg :: String -> (Term -> b) -> ParsecT String Context Identity b
parseOneArg keyword constructor = reserved keyword >>
                                  liftM constructor parseTerm

parseSucc :: ParsecT String Context Identity Term
parseSucc = parseOneArg "succ" TrmSucc

parsePred :: ParsecT String Context Identity Term
parsePred = parseOneArg "pred" TrmPred

parseIsZero :: ParsecT String Context Identity Term
parseIsZero = parseOneArg "iszero" TrmIsZero

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

parseVar :: ParsecT String Context Identity Term
parseVar = do
    varName <- identifier
    if isUpper $ head varName
      then fail "variables must start with a lowercase letter"
      else do
           context <- getState
           idx <- throwsToParser $ indexOf varName context
           return $ TrmVar idx (ctxLength context)

parseVarBind :: ParsecT String Context Identity Term
parseVarBind = do
    var <- identifier
    _ <- colon
    ty <- parseType
    let binding = VarBind ty
    updateState $ appendBinding var binding
    return (TrmBind var binding)

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
            try parseIsZero <|>
            parseIf <|>
            parseNat <|>
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

-- Top-level routine that is responsible for parsing the provided source text.
-- Terms will be returned or an error will be raised if the parse fails. Setup
-- new top-level context when running parser.
parseTLBN :: Monad m => String -> m Term
parseTLBN srcStr =
    case runParser parseTerms newContext "TLBN Parser" srcStr of
      Left err   -> error (show err)
      Right term -> return term

