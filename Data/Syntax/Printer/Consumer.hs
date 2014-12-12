{-# LANGUAGE DeriveFunctor #-}
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
import Control.Monad
import Control.Monad.Trans.Error ()
import Data.Bifunctor.Apply
import Data.Monoid

-- | A writer monad combined with Either String.
newtype Consumer m a = Consumer { runConsumer :: Either String (m, a) }
    deriving (Functor)

instance Monoid m => Applicative (Consumer m) where
    pure x = Consumer $ Right (mempty, x)
    f <*> x = Consumer $ bilift2 (<>) ($) <$> runConsumer f <*> runConsumer x

instance Monoid m => Alternative (Consumer m) where
    empty = Consumer $ empty
    f <|> g = Consumer $ runConsumer f <|> runConsumer g

instance Monoid m => Monad (Consumer m) where
    return = pure
    m >>= f = Consumer $ do
        (m1, x) <- runConsumer m
        (m2, y) <- runConsumer (f x)
        return (m1 <> m2, y)

instance Monoid m => MonadPlus (Consumer m) where
    mzero = empty
    mplus = (<|>)
