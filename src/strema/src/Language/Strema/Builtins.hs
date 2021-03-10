{- | Builtin stuff

Includes:

- builtin types
- values
- datatypes
- function implementations

-}

{-# language OverloadedStrings #-}

module Language.Strema.Builtins
  ( -- * Types
    tUnit
  , tInt
  , tFloat
  , tString
  , tBool
    -- * Values
  , true
  , false
  , unit
    -- * Data types
  , builtinDatatypes
  , dataBool
  , dataOption
    -- * Functions
  , Builtins
  , Builtin(..)
  , Impl(..)
  , builtins
  )
  where

import qualified Data.Text as T
import qualified Data.Map as M

import Language.Strema.Syntax.Ast

-- * Types

tUnit :: Type
tUnit = TypeRec mempty

tInt :: Type
tInt = TypeCon "Int"

tFloat :: Type
tFloat = TypeCon "Float"

tString :: Type
tString = TypeCon "String"

tBool :: Type
tBool = TypeCon "Bool"

-- * Values

true :: Expr ()
true = EVariant (Variant "True" unit)

false :: Expr ()
false = EVariant (Variant "False" unit)

unit :: Expr ()
unit = ERecord mempty

-- * Data types

builtinDatatypes :: [Datatype]
builtinDatatypes =
  [ dataBool
  , dataOption
  ]

dataBool :: Datatype
dataBool =
  Datatype "Bool" []
    [ Variant "True" tUnit
    , Variant "False" tUnit
    ]

dataOption :: Datatype
dataOption =
  Datatype "Option" ["a"]
    [ Variant "Some" (TypeVar "a")
    , Variant "None" tUnit
    ]

-- * Functions

-- | A Built-in function
data Builtin
  = Builtin
    { bName :: Var -- ^ the name
    , bType :: Type -- ^ the type
    , bImpl :: Impl -- ^ the implementation
    }

-- | The type of the function and it's
--   implementation in the target source code
data Impl
  = Func T.Text
  | BinOp T.Text

type Builtins = M.Map Var Builtin

-- | All of the builtin functions
builtins :: Builtins
builtins = M.unions
  [ ints
  , bools
  , strings
  ]

-- | Integer builtins
ints :: Builtins
ints = M.fromList
  [ binop "add" binInt "+"
  , binop "sub" binInt "-"
  , binop "mul" binInt "*"
  , binop "div" binInt "/"
  , func "negate" (TypeFun [tInt] tInt)
    "function (x) { return 0 - x; }"

  , binop "int_equals" (TypeFun [tInt, tInt] tBool)
    "==="
  ]
  where
    binInt = TypeFun [tInt, tInt] tInt

-- | Boolean builtins
bools :: Builtins
bools = M.fromList
  [ binop "and" (TypeFun [tBool, tBool] tBool)
    "&&"
  , binop "or" (TypeFun [tBool, tBool] tBool)
    "||"
  , func "not" (TypeFun [tBool] tBool)
    "function (x) { return !x; }"
  ]

-- | String builtins
strings :: Builtins
strings = M.fromList
  [ binop "concat" (TypeFun [tString, tString] tString)
    "+"
  ]

-- | Binary operator helper constructor
binop :: Var -> Type -> T.Text -> (Var, Builtin)
binop name typ impl =
  (name, Builtin name typ (BinOp impl))

-- | Function helper constructor
func :: Var -> Type -> T.Text -> (Var, Builtin)
func name typ impl =
  (name, Builtin name typ (Func impl))
