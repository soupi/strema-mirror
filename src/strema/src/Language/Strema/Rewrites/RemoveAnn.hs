{- | Remove annotations from the AST

This rewrite will remove annotations from the AST,
this makes it easier to test ASTs when comparing
to hand written ASTs.

We'll use uniplate, which provides traversal
and manipulation functions on ADTs that have instances
of the Data typeclass.

for example,

@
transform :: Data on => (on -> on) -> on -> on
@

Will run a function on all nodes of @on@ from the bottom up.

@
transformBi :: (Data to, Data from) => (to -> to) -> from -> from
@

is a variant of @transform@ that does that to every node of
@to@ inside a larger type @from@.

For us, @to@ will be @Expr a@ and @from@ will be @File a@,
because we want to remove all of the @EAnnotated@ nodes.

-}

{-# language ScopedTypeVariables #-}

module Language.Strema.Rewrites.RemoveAnn where

import Language.Strema.Syntax.Ast

import Data.Data (Data)
import qualified Data.Generics.Uniplate.Data as U

removeAnn :: Data a => File a -> File ()
removeAnn = removeAnn'

removeAnn' :: Functor f => Data a => Data (f ()) => f a -> f ()
removeAnn' x =
  let
    x' = fmap (const ()) x
    removeEAnnotated :: Data (f ()) => f () -> f ()
    removeEAnnotated =
      U.transformBi $ \case
        EAnnotated (_ :: ()) e -> e
        other -> other
  in
    removeEAnnotated x'
