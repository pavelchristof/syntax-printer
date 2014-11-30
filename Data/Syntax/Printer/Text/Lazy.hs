{-# LANGUAGE MultiParamTypeClasses #-}
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
    runPrinter
    )
    where

import           Control.Monad
import           Data.SemiIsoFunctor
import           Data.Syntax
import           Data.Syntax.Char
import           Data.Syntax.Printer.Consumer
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import qualified Data.Text.Lazy.Builder.Scientific as B

-- | Prints a value to a Text Builder using a syntax description.
newtype Printer a = Printer { getConsumer :: Consumer Builder a }
    deriving (SemiIsoFunctor, SemiIsoApply, SemiIsoAlternative, SemiIsoMonad)

instance Syntax Printer Text where
    anyChar = Printer . Consumer $ Right . singleton
    take n = Printer . Consumer $ Right . fromLazyText . T.take (fromIntegral n)
    takeWhile p = Printer . Consumer $ Right . fromLazyText . T.takeWhile p
    takeWhile1 p = Printer . Consumer $ Right . fromLazyText <=< notNull . T.takeWhile p
      where notNull t | T.null t  = Left "takeWhile1 failed"
                      | otherwise = Right t
    takeTill1 p = Printer . Consumer $ Right . fromLazyText <=< notNull . T.takeWhile (not . p)
      where notNull t | T.null t  = Left "takeTill1 failed"
                      | otherwise = Right t

instance SyntaxChar Printer Text where
    decimal = Printer . Consumer $ Right . B.decimal
    hexadecimal = Printer . Consumer $ Right . B.hexadecimal
    realFloat = Printer . Consumer $ Right . B.realFloat
    scientific = Printer . Consumer $ Right . B.scientificBuilder

-- | Runs the printer.
runPrinter :: Printer a -> a -> Either String Builder
runPrinter = runConsumer . getConsumer
