{-| Parsing Strema

Here's an example Strema program that we are going to parse:

@
type List a =
    | Nil {}
    | Cons { head : a, tail : List a }
end

def length(xs) := do
    case xs of
        | Nil {} -> 0
        | Cons { head = _, tail = rest } -> do
            def res := add(1, length(rest))
            res
        end
    end
end

def main() := do
    ffi("console.log", length(Cons { head = 1, tail = Cons { head = 2, tail = Nil {} } }))
end
@

-}

{-# language OverloadedStrings #-}

module Language.Strema.Syntax.Parser where

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

import Language.Strema.Syntax.Ast

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
    P.runParser
      (lexeme (pure ()) *> newlines *> lexeme p <* newlines <* P.eof)
      file
      src

-- * Lexing

-- | How to read whitespace after reading a node
sc :: Parser ()
sc = L.space
  P.hspace1 -- We don't want to read newlines because they matter semantically
  (L.skipLineComment "//")
  (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- * newlines

newline :: Parser ()
newline =
  lexeme (void P.newline P.<|> void P.crlf) <?> "a newline"

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
equals = symbol "=" *> newlines

arrow :: Parser ()
arrow = symbol "->" *> newlines

underscore :: Parser ()
underscore = symbol "_"

comma :: Parser ()
comma = symbol ","

bar :: Parser ()
bar = symbol "|"

harddot :: Parser ()
harddot = void $ P.char '.'

colon :: Parser ()
colon = symbol ":"

assign :: Parser ()
assign = symbol ":=" *> newlines

between :: Char -> Char -> Parser a -> Parser a
between open close p = do
  void $ lexeme (P.char open) *> newlines
  result <- p
  void (P.char close)
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
  [ "def"
  , "do"
  , "end"
  , "fun"
  , "case"
  , "of"
  , "ffi"
  ]


rword :: Text -> Parser ()
rword rw =
  (void . lexeme . P.try) (P.string rw <* P.notFollowedBy nameRest) <?> T.unpack rw

reservedWord :: Parser ()
reservedWord =
  P.choice (map rword reservedWords)

var :: Parser Text
var = lowername <?> "a variable"

typevar :: Parser Text
typevar = lowername <?> "a type variable"

typename :: Parser Text
typename = uppername <?> "a type name"

lowername :: Parser Text
lowername = name' P.lowerChar

uppername :: Parser Text
uppername = name' P.upperChar

name' :: Parser Char -> Parser Text
name' begin = do
  P.notFollowedBy reservedWord
  c <- begin
  rest <- P.many nameRest
  pure $ T.pack $ c : rest

nameRest :: Parser Char
nameRest =
  P.choice
    [ P.alphaNumChar
    , P.char '_'
    ]


-- * Parser

parseFile :: Parser (File Ann)
parseFile = do
  File <$> P.many (lexeme parseDef <* (newlines1 P.<|> P.eof))

parseDef :: Parser (Definition Ann)
parseDef = do
  ann <- getAnn
  P.choice
    [ TermDef ann <$> parseTermDef
    , TypeDef ann <$> parseTypeDef
    ]

-- * Types


parseTypeDef :: Parser Datatype
parseTypeDef = do
  rword "type"
  name <- lexeme typename <* newlines
  args <- P.many
    (lexeme typevar <* newlines)
  equals *> newlines
  typ <- P.many
    (bar *> lexeme (parseVariant parseType) <* newlines)
  rword "end"
  pure $ Datatype name args typ

parseType :: Parser Type
parseType = do
  t <- (lexeme parseType' <* newlines)
  args <- P.many (lexeme parseType' <* newlines)
  pure $ foldl' TypeApp t args

parseType' :: Parser Type
parseType' =
  P.choice
    [ parens parseType
    , TypeVar <$> typevar
    , TypeCon <$> typename
    , TypeFun
      <$> (brackets $ P.sepBy (lexeme parseType <* newlines) comma)
      <*> (arrow *> parseType)
      <?> "type function"
    , TypeRec . M.toList . fst
      <$> parseRecord colon parseType Nothing
      <?> "type record"
    ]

-- * Terms

parseTermDef :: Parser (TermDef Ann)
parseTermDef = do
  rword "def"
  name <- lexeme var
  margs <- P.optional $ lexeme $ parens $ P.sepBy (lexeme var <* newlines) comma
  assign
  maybe
    (Variable name <$> parseExpr)
    (\args -> Function name args <$> parseSub)
    margs

parseSub :: Parser (Sub Ann)
parseSub =
  P.choice
    [ do
      rword "do" *> newlines
      stmts <- P.some (lexeme parseStmt <* (newlines1 P.<|> P.eof))
      rword "end"
      pure stmts
    , pure . SExpr <$> parseExpr
    ]

parseStmt :: Parser (Statement Ann)
parseStmt = do
  ann <- getAnn
  P.choice
    [ SDef ann <$> parseTermDef <?> "a definition"
    , SExpr <$> parseExpr <?> "an expression"
    ]

-- * Expr

parseExpr :: Parser (Expr Ann)
parseExpr = do
  ann <- getAnn
  e <- parseExpr'

  margs <- P.optional $
    parens $ P.sepBy (lexeme parseExpr <* newlines) comma

  mlabels <- (<?> "record accessor") $
    P.optional $ do
      P.notFollowedBy P.space1
      harddot
      P.sepBy (var <?> "a label") harddot

  let
    e' = EAnnotated ann
      $ maybe e (EFunCall e) margs

  pure $ maybe
    e'
    (EAnnotated ann . foldl' ERecordAccess e')
    mlabels

parseExpr' :: Parser (Expr Ann)
parseExpr' =
  (<?> "an expression") $ P.choice
    [ parens parseExpr <?> "parenthesis"
    , parseFfi <?> "an ffi call"
    , parseCaseOf <?> "a case expression"
    , parseLambda <?> "a lambda"
    , (<?> "a record") $ do
      (record, mext) <- parseRecord equals parseExpr (Just parseExpr)
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

parseCaseOf :: Parser (Expr Ann)
parseCaseOf = do
  rword "case" *> newlines
  e <- lexeme parseExpr
  rword "of" *> newlines
  pats <- parsePatterns
  rword "end"
  pure $ ECase e pats

parsePatterns :: Parser [(Pattern, Sub Ann)]
parsePatterns = P.many $
  (,)
    <$> (bar *> lexeme parsePattern <* newlines)
    <*> (arrow *> lexeme parseSub <* newlines)

parsePattern :: Parser Pattern
parsePattern =
  (<?> "a pattern") $ P.choice
    [ parens parsePattern
    , PWildcard <$ underscore
    , PVar <$> var
    , PLit <$> parseLit
    , PRecord . fst
      <$> parseRecord equals parsePattern Nothing
      <?> "a record pattern"
    , PVariant
      <$> parseVariant (lexeme parsePattern <* newlines)
      <?> "a variant pattern"
    ]

parseFfi :: Parser (Expr Ann)
parseFfi = do
  rword "ffi"
  parens $ do
    EFfi
      <$> (lexeme stringLiteral <* comma)
      <*> P.sepBy (lexeme parseExpr <* newlines) comma

parseLambda :: Parser (Expr Ann)
parseLambda = do
  rword "fun"
  args <- lexeme $ parens $ P.sepBy (lexeme var <* newlines) comma
  arrow *> newlines
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
    <$> (lexeme uppername <* newlines <?> "data constructor")
    <*> p

parseRecord :: Parser () -> Parser a -> Maybe (Parser b) -> Parser (Record a, Maybe b)
parseRecord pSym pa mpb = braces $ do
  -- we are going to allow records with duplicate
  -- labels. Semantically, this means that
  -- the first label matches and the rest
  -- are discarded.
  record <- fmap
    (M.fromListWith $ flip const) $ lexeme $
      P.sepBy
        ( do
          l <- lexeme lowername <?> "a label"
          pSym *> newlines
          (,) l <$> (lexeme pa <* newlines)
        )
        comma

  ext <-
    case mpb of
      Just pb ->
        P.optional $ bar *> lexeme pb <* newlines
      Nothing ->
        pure Nothing

  pure (record, ext)
    
