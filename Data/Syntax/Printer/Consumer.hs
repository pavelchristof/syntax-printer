{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{- |
Module      :  Data.Syntax.Printer.Consumer
Description :  Common base for both Text and ByteString printers.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  Paweł Nowak <pawel834@gmail.com>
Stability   :  experimental
-}
module Data.Syntax.Printer.Consumer where

import Control.Applicative
import Control.Category
import Control.Category.Structures
import Control.Lens.SemiIso
import Control.Monad
import Control.SIArrow
import Data.Bitraversable
import Data.Monoid
import Prelude hiding (id, (.))

-- | A category that consumes values using a monoid.
newtype Consumer m a b = Consumer { runConsumer :: b -> Either String (m, a) }

instance Monoid m => Category (Consumer m) where
    id = Consumer $ \b -> Right (mempty, b)
    Consumer f . Consumer g = Consumer $ \c -> do
         (m1, b) <- f c
         (m2, a) <- g b
         return (m1 <> m2, a)

instance Monoid m => Products (Consumer m) where
    Consumer f *** Consumer g = Consumer $ \(c, d) -> do
         (m1, a) <- f c
         (m2, b) <- g d
         return (m1 <> m2, (a, b))

instance Monoid m => Coproducts (Consumer m) where
    Consumer f +++ Consumer g = Consumer $ fmap bisequenceA . bimapM f g

instance Monoid m => CatPlus (Consumer m) where
    cempty = Consumer (const (Left "cempty"))
    Consumer f /+/ Consumer g = Consumer $ \c -> f c <|> g c

instance Monoid m => SIArrow (Consumer m) where
    siarr ai = Consumer $ fmap (mempty, ) . unapply ai
