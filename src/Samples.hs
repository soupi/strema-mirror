{- | Strema sample programs
-}

{-# language OverloadedStrings #-}

module Samples where

import qualified Data.Map as M

import Strema.Ast as Strm

boilerplate :: Expr () -> File ()
boilerplate e = File
  [ TermDef $ Function "main" []
    [ SExpr $ EFfi "console.log" [ e ]
    ]
  ]

simple :: File ()
simple = boilerplate $
  ELit $ LInt 7

variant :: File ()
variant = boilerplate $
  EVariant (Variant "Nil" (ERecord mempty))

-- expected output: 1
patmatch1 :: File ()
patmatch1 = boilerplate $
  ECase (ELit $ LInt 0)
    [ (PWildcard, ELit $ LInt 1)
    ]

-- expected output: 0
patmatch2 :: File ()
patmatch2 = boilerplate $
  ECase (ELit $ LInt 0)
    [ (PLit (LInt 1), ELit $ LInt 1)
    , (PLit (LInt 0), ELit $ LInt 0)
    ]

-- expected output: 17
patmatch3 :: File ()
patmatch3 = boilerplate $
  ECase (ELit $ LInt 17)
    [ (PLit (LInt 1), ELit $ LInt 1)
    , (PLit (LInt 0), ELit $ LInt 0)
    , (PVar "v", EVar "v")
    ]

-- expected output: 0
patmatch4 :: File ()
patmatch4 = boilerplate $
  ECase
    ( EVariant $ Variant "Nil" $
      ERecord $ M.fromList
        [ ("head", ELit $ LInt 0)
        , ("tail", ERecord mempty)
        ]
    )
    [ (PVariant $ Variant "Nil" (PVar "obj"), ERecordAccess (EVar "obj") "head")
    ]

-- expected output: 0
patmatch5 :: File ()
patmatch5 = boilerplate $
  ECase
    ( EVariant $ Variant "Nil" $
      ERecord $ M.fromList
        [ ("head", ELit $ LInt 0)
        , ("tail", ERecord mempty)
        ]
    )
    [ ( PVariant $ Variant "Nil"
        ( PRecord $ M.fromList
          [ ("head", PVar "head")
          , ("tail", PRecord mempty)
          ]
        )
      , EVar "head"
      )
    ]

-- expected output: 7
recordExt :: File ()
recordExt = boilerplate $
  ERecordAccess
    ( ERecordExtension
      ( M.fromList
        [ ("x", ELit $ LInt 7)
        ]
      )
      (ERecord mempty)
    )
    "x"

-- expected output: 2
addition :: File ()
addition = boilerplate $
  EFunCall (EVar "add") [ ELit $ LInt 1, ELit $ LInt 1 ]
