{-| Parsing Strema

Here's an example Strema program that we are going to parse:

@
datatype List a =
    | Nil {}
    | Cons { head = a, tail = List a }
end

fun length(xs): do
    case xs of
        | Nil {} -> 0
        | Cons { head = _, tail = rest } -> do
            let res = add(1, length(rest))
            res
        end
    end
end

fun main(): do
    ffi("console.log", length(Cons { head = 1, tail = Cons { head = 2, tail = Nil {} } }))
end
@

-}

{-# language OverloadedStrings #-}

module Strema.Parser where

import Control.Monad
import Data.Bifunctor (first)
import Data.Void (Void)
import Data.Text (Text)
import Data.Foldable (foldl')
import qualified Data.Text as T
import qualified Data.Map as M
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

import Strema.Ast

-- * Types

type Parser = P.Parsec Void Text

type Ann = P.SourcePos

-- * Utils

getAnn :: Parser Ann
getAnn = P.getSourcePos

-- * Run

runParser :: Parser a -> FilePath -> Text -> Either Text a
runParser p file src =
  first (T.pack . P.errorBundlePretty) $
    P.runParser (p <* P.eof) file src

-- * Lexing

-- | How to read whitespace after reading a node
sc :: Parser ()
sc = L.space
  P.hspace1 -- We don't want to read newlines because it matters semantically
  (L.skipLineComment "//")
  (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- * newlines

newline :: Parser ()
newline =
  lexeme (void P.newline P.<|> void P.crlf)

newlines :: Parser ()
newlines =
  void $ P.many newline

newlines1 :: Parser ()
newlines1 =
  void $ P.some newline

-- * symbols

symbol :: Text -> Parser ()
symbol = void . L.symbol sc

equals :: Parser ()
equals = symbol "="

arrow :: Parser ()
arrow = symbol "->"

comma :: Parser ()
comma = symbol ","

bar :: Parser ()
bar = symbol "|"

harddot :: Parser ()
harddot = void $ P.char '.'

colon :: Parser ()
colon = symbol ":"

between :: Char -> Char -> Parser a -> Parser a
between open close p = do
  void $ lexeme (P.char open) *> newlines
  result <- p
  void $ lexeme (P.char close) *> newlines
  pure result

parens :: Parser a -> Parser a
parens = between '(' ')'

brackets :: Parser a -> Parser a
brackets = between '[' ']'

braces :: Parser a -> Parser a
braces = between '{' '}'

-- * atoms

stringLiteral :: Parser Text
stringLiteral =
  fmap T.pack $ P.char '\"' *> P.manyTill L.charLiteral (P.char '\"')

number :: Parser Lit
number = do
  sign <- P.optional $ P.char '-'
  num1 <- P.some P.digitChar
  num2 <- P.optional $ P.char '.' *> P.some P.digitChar
  case num2 of
    Nothing ->
      pure $ LInt (read $ maybe [] pure sign <> num1)
    Just n ->
      pure $ LFloat (read $ maybe [] pure sign <> num1 <> "." <> n)

reservedWords :: [Text]
reservedWords =
  [ "let"
  , "fun"
  , "do"
  , "end"
  , "case"
  , "of"
  , "datatype"
  , "ffi"
  ]

rword :: Text -> Parser ()
rword rw =
  (void . lexeme . P.try) (P.string rw <* P.notFollowedBy P.alphaNumChar)

reservedWord :: Parser ()
reservedWord =
  P.choice (map rword reservedWords)

var :: Parser Text
var = lowername <?> "a variable"

lowername :: Parser Text
lowername = name' P.lowerChar

uppername :: Parser Text
uppername = name' P.upperChar

name' :: Parser Char -> Parser Text
name' begin = lexeme $ do
  P.notFollowedBy reservedWord
  c <- begin
  rest <- P.many $ P.choice
    [ P.alphaNumChar
    , P.oneOf ['_', '\'', '?']
    ]
  pure $ T.pack (c : rest)

-- * Parser

parseFile :: Parser (File Ann)
parseFile = do
  File <$> P.many parseDef

parseDef :: Parser (Definition Ann)
parseDef = do
  P.choice
    [ TermDef <$> parseTermDef
    ]

parseTermDef :: Parser (TermDef Ann)
parseTermDef =
  P.choice
    [ do
      rword "let"
      v <- lexeme var
      equals
      e <- parseExpr
      newlines
      pure $ Variable v e
    ]

parseSub :: Parser (Sub Ann)
parseSub =
  P.choice
    [ do
      rword "do" *> newlines
      stmts <- P.some (lexeme parseStmt <* newlines1)
      rword "end" *> newlines
      pure stmts
    , pure . SExpr <$> (parseExpr <* newlines)
    ]

parseStmt :: Parser (Statement Ann)
parseStmt =
  P.choice
    [ SDef <$> parseTermDef
    , SExpr <$> parseExpr
    ]

-- * Expr

parseExpr :: Parser (Expr Ann)
parseExpr = lexeme $ do
  ann <- getAnn
  e <- parseExpr'

  mspace <- P.optional P.space1

  margs <- P.optional $
    parens $ P.sepBy parseExpr comma

  mext <-
    case mspace of
      Nothing ->
        P.optional $ do
          harddot
          P.sepBy (var <?> "a label") harddot
      Just{} ->
        pure Nothing

  let
    e' = EAnnotated ann
      $ maybe e (EFunCall e) margs

  pure $ maybe
    e'
    (EAnnotated ann . foldl' ERecordAccess e')
    mext

parseExpr' :: Parser (Expr Ann)
parseExpr' =
  P.choice
    [ parens parseExpr <?> "parenthesis"
    , parseFfi <?> "an ffi call"
    , parseLambda <?> "a lambda"
    , (<?> "a record") $ do
      (record, mext) <- parseRecord parseExpr parseExpr
      pure $ maybe
        (ERecord record)
        (ERecordExtension record)
        mext
    , EVariant
      <$> parseVariant parseExpr
      <?> "a variant"
    , ELit <$> parseLit <?> "a literal"
    , EVar <$> var
    ]

parseFfi :: Parser (Expr Ann)
parseFfi = do
  rword "ffi"
  parens $ do
    EFfi
      <$> (lexeme stringLiteral <* comma)
      <*> P.sepBy parseExpr comma

parseLambda :: Parser (Expr Ann)
parseLambda = do
  rword "fun"
  args <- parens $ P.sepBy var comma
  lexeme colon *> newlines
  sub <- parseSub
  pure $ EFun args sub

-- * General

parseLit :: Parser Lit
parseLit =
  P.choice
    [ number
    , LString <$> stringLiteral
    ]

parseVariant :: Parser a -> Parser (Variant a)
parseVariant p =
  Variant
    <$> (uppername <?> "data constructor")
    <*> p

parseRecord :: Parser a -> Parser b -> Parser (Record a, Maybe b)
parseRecord pa pb = braces $ do
  -- we are going to allow records with duplicate
  -- labels. Semantically, this means that
  -- the first label matches and the rest
  -- are discarded.
  record <- fmap
    (M.fromListWith $ flip const) $
      P.sepBy
        ( do
          l <- lexeme lowername <?> "a label"
          equals *> newlines
          (,) l <$> (lexeme pa <* newlines)
        )
        comma

  ext <- P.optional $ do
    bar
    pb

  pure (record, ext)
    
