{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      :  Data.Syntax.Printer.Text.Lazy
Description :  Prints to a Text Builder using lazy Text as the sequence.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  Paweł Nowak <pawel834@gmail.com>
Stability   :  experimental
-}
module Data.Syntax.Printer.Text.Lazy (
    Printer,
    runPrinter,
    runPrinter_
    )
    where

import           Control.Category
import           Control.Category.Structures
import           Control.Monad
import           Control.SIArrow
import           Data.Syntax
import           Data.Syntax.Char
import           Data.Syntax.Printer.Consumer
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import qualified Data.Text.Lazy.Builder.Scientific as B
import           Prelude hiding (id, (.))

-- | Prints a value to a Text Builder using a syntax description.
newtype Printer a b = Printer { getConsumer :: Consumer Builder a b }
    deriving (Category, Products, Coproducts, CatPlus, SIArrow)

wrap :: (b -> Either String Builder) -> Printer () b
wrap f = Printer $ Consumer $ \b -> fmap (, ()) (f b)

instance Syntax Printer where
    type Seq Printer = Text
    anyChar = wrap $ Right . singleton
    take n = wrap $ Right . fromLazyText . T.take (fromIntegral n)
    takeWhile p = wrap $ Right . fromLazyText . T.takeWhile p
    takeWhile1 p = wrap $ Right . fromLazyText <=< notNull . T.takeWhile p
      where notNull t | T.null t  = Left "takeWhile1 failed"
                      | otherwise = Right t
    takeTill1 p = wrap $ Right . fromLazyText <=< notNull . T.takeWhile (not . p)
      where notNull t | T.null t  = Left "takeTill1 failed"
                      | otherwise = Right t

instance SyntaxChar Printer where
    decimal = wrap $ Right . B.decimal
    hexadecimal = wrap $ Right . B.hexadecimal
    realFloat = wrap $ Right . B.realFloat
    scientific = wrap $ Right . B.scientificBuilder

-- | Runs the printer.
runPrinter :: Printer a b -> b -> Either String (Builder, a)
runPrinter = runConsumer . getConsumer

-- | Runs the printer and discards the result.
runPrinter_ :: Printer a b -> b -> Either String Builder
runPrinter_ = (fmap fst .) . runConsumer . getConsumer
