module Parser (parseTLBN) where

-- Adapted from TAPL simplebool parser code.
-- Parses a file contain expressions of the TLBN language.  

import Text.ParserCombinators.Parsec
import qualified Control.Monad as CMonad
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity (Identity)
import TLBN

-- What a comment looks like. Does not allow nested comments
comment :: ParsecT String u Identity ()
comment = do
    _ <- string "/*"
    _ <- many (noneOf "*" <|> starNotCommentEnd)
    _ <- commentEnd
    return ()

-- We need to use "try", because we want this to fail when
-- it encounters the end of the comment, and give back the
-- last star.  "commentEnd" below handles the case of extra
-- stars before the final "*/"
starNotCommentEnd :: GenParser Char st Char
starNotCommentEnd = try (many1 (char '*') >> noneOf "/")

commentEnd :: ParsecT String u Identity Char
commentEnd = many1 (char '*') >> char '/'

separator :: ParsecT String () Identity ()
separator = CMonad.void space <|> 
            comment

separators :: ParsecT String () Identity [()]
separators = many separator
             
separators1 :: ParsecT String () Identity [()]
separators1 = many1 separator

-- --------------------
-- Zero-arg terms
parseTrue :: Parser Term
parseTrue = string "true" >> return TrmTru

parseFalse :: Parser Term
parseFalse = string "false" >> return TrmFls

parseNat :: ParsecT String u Identity Term
parseNat = CMonad.liftM (numToSucc . read) $ many1 digit

parseIdent :: Parser Term
parseIdent = CMonad.liftM (stringToVar . read) $ many1 letter

parseNatType :: ParsecT String u Identity Type
parseNatType = string "Nat" >> return TyNat

parseBoolType :: ParsecT String u Identity Type
parseBoolType = string "Bool" >> return TyBool

parseType :: ParsecT String u Identity Type
parseType = parseNatType <|>
            parseBoolType

numToSucc :: Integer -> Term
numToSucc 0 = TrmZero
numToSucc n = TrmSucc $ numToSucc (n - 1)

stringToVar :: String -> Term
stringToVar = TrmIdent

-- --------------------
-- One-arg terms
parseOneArgTerm :: String -> (Term -> b) -> ParsecT String () Identity b
parseOneArgTerm keyword constructor = 
    string keyword >> separators1 >> CMonad.liftM constructor parseTerm

parseSucc :: ParsecT String () Identity Term
parseSucc = parseOneArgTerm "succ" TrmSucc

parsePred :: ParsecT String () Identity Term
parsePred = parseOneArgTerm "pred" TrmPred

parseIsZero :: ParsecT String () Identity Term
parseIsZero = parseOneArgTerm "iszero" TrmIsZero

surroundedBy :: ParsecT String () Identity t ->
                ParsecT String () Identity t1 ->
                ParsecT String () Identity b ->
                ParsecT String () Identity b
surroundedBy open close parser = do
    _ <- open
    _ <- separators
    output <- parser
    _ <- separators
    _ <- close
    return output

-- Responsible for parsing if ifPred then conseq else alt fi statements.
parseIf :: ParsecT String () Identity Term
parseIf = do
    _ <- string "if"
    _ <- separators1
    ifPred <- parseTerm
    _ <- separators1
    _ <- string "then"
    _ <- separators1
    conseq <- parseTerm
    _ <- separators1
    _ <- string "else"
    _ <- separators1
    alt <- parseTerm
    _ <- separators1
    _ <- string "fi"
    return $ TrmIf ifPred conseq alt

-- Parses: abs lpar identifier colon Type fullstop Term rpar
parseAbs :: ParsecT String () Identity Term
parseAbs = do
    _ <- string "abs"
    _ <- separators1
    _ <- string "("
    var <- parseIdent
    _ <- separators1
    _ <- string ":"
    _ <- separators1
    ty <- parseType
    _ <- separators1
    _ <- string "."
    _ <- separators1
    body <- parseTerm
    _ <- string ")"
    return (TrmAbs var ty body)

-- Parses a term in our language.
parseTerm :: ParsecT String () Identity Term
parseTerm = parseTrue <|>
            parseFalse <|>
            parseSucc <|>
            parsePred <|>
            parseIsZero <|>
            try parseIf <|>
            parseNat <|>
            parseAbs <|>
            parseIdent <|>
            surroundedBy (char '(') (char ')') parseTerm

-- What the end of a term look like.
termEnd :: Parser ()
termEnd = separators >> eof

-- The TLBN parser top level.
tlbnParser :: Parser [Term]
tlbnParser = separators >>
              endBy1 parseTerm termEnd

-- Top-level routine that is responsible for parsing the provided text. Terms
-- will be retured or an error will be raised if the parse fails.
parseTLBN :: Monad m => String -> m [Term]
parseTLBN src =
    case parse tlbnParser "TLBN Parser" src of
      Left err -> error (show err)
      Right term -> return term
