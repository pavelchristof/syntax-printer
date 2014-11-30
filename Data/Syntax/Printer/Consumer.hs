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
import Control.Lens.SemiIso
import Control.Monad
import Data.Monoid
import Data.SemiIsoFunctor

-- | A contravariant functor that consumes values using a monoid.
newtype Consumer m a = Consumer { runConsumer :: a -> Either String m }

instance SemiIsoFunctor (Consumer m) where
    simap f (Consumer p) = Consumer $ apply f >=> p

instance Monoid m => SemiIsoApply (Consumer m) where
    siunit = Consumer $ \_ -> Right mempty
    Consumer f /*/ Consumer g = Consumer $ \(a, b) -> (<>) <$> f a <*> g b

instance Monoid m => SemiIsoAlternative (Consumer m) where
    siempty = Consumer $ \_ -> Left "siempty"
    Consumer f /|/ Consumer g = Consumer $ \a -> f a <|> g a

instance Monoid m => SemiIsoMonad (Consumer m) where
    Consumer m //= f = Consumer $ \(a, b) -> (<>) <$> m a <*> runConsumer (f a) b
