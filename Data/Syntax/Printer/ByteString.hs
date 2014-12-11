{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      :  Data.Syntax.Printer.ByteString
Description :  Prints to a ByteString Builder using strict ByteString as the sequence.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  Paweł Nowak <pawel834@gmail.com>
Stability   :  experimental
-}
module Data.Syntax.Printer.ByteString (
    Printer,
    runPrinter,
    runPrinter_
    )
    where

import           Control.Category
import           Control.Monad
import           Control.SIArrow
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy.Builder
import           Data.Syntax
import           Data.Syntax.Printer.Consumer
import           Prelude hiding (id, (.))

-- | Prints a value to a Text Builder using a syntax description.
newtype Printer a b = Printer { getConsumer :: Consumer Builder a b }
    deriving (Category, Products, Coproducts, CategoryPlus, SIArrow)
    
wrap :: (b -> Either String Builder) -> Printer () b
wrap f = Printer $ Consumer $ \b -> fmap (, ()) (f b)

instance Syntax Printer where
    type Seq Printer = ByteString
    anyChar = wrap $ Right . word8
    take n = wrap $ Right . byteString . BS.take n
    takeWhile p = wrap $ Right . byteString . BS.takeWhile p
    takeWhile1 p = wrap $ Right . byteString <=< notNull . BS.takeWhile p
      where notNull t | BS.null t  = Left "takeWhile1 failed"
                      | otherwise = Right t
    takeTill1 p = wrap $ Right . byteString <=< notNull . BS.takeWhile (not . p)
      where notNull t | BS.null t  = Left "takeTill1 failed"
                      | otherwise = Right t

-- | Runs the printer.
runPrinter :: Printer a b -> b -> Either String (Builder, a)
runPrinter = runConsumer . getConsumer

-- | Runs the printer and discards the result.
runPrinter_ :: Printer a b -> b -> Either String Builder
runPrinter_ = (fmap fst .) . runConsumer . getConsumer
