{- |

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#keywords

-}

{-# language OverloadedStrings #-}

module JS.ReservedWords where

import Utils

reservedWords :: [Text]
reservedWords =
  [ "break"
  , "case"
  , "catch"
  , "class"
  , "const"
  , "continue"
  , "debugger"
  , "default"
  , "delete"
  , "do"
  , "else"
  , "export"
  , "extends"
  , "finally"
  , "for"
  , "function"
  , "if"
  , "import"
  , "in"
  , "instanceof"
  , "new"
  , "return"
  , "super"
  , "switch"
  , "this"
  , "throw"
  , "try"
  , "typeof"
  , "var"
  , "void"
  , "while"
  , "with"
  , "yield"
  
  , "abstract"
  , "boolean"
  , "byte"
  , "char"
  , "double"
  , "final"
  , "float"
  , "goto"
  , "int"
  , "long"
  , "native"
  , "short"
  , "synchronized"
  , "throws"
  , "transient"
  , "volatile"
  ]
