{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CLaSH.Netlist.BlackBox.Types where

import Control.Monad.Writer (MonadWriter,Writer)
import Data.Text.Lazy (Text)

import CLaSH.Netlist.Types

data BlackBoxContext
  = Context
  { result    :: SyncIdentifier
  , inputs    :: [SyncIdentifier]
  , litInputs :: [Identifier]
  , funInputs :: [(Line,BlackBoxContext)]
  }
  deriving Show

type SyncIdentifier = Either Identifier (Identifier,Identifier)

type Line = [Element]

data Element = C  Text
             | D Decl
             | O
             | I   Int
             | L   Int
             | Sym Int
             | Clk (Maybe Int)
             | Rst (Maybe Int)
  deriving Show

data Decl = Decl Int [Line]
  deriving Show

newtype BlackBoxMonad a = B { runBlackBoxM :: Writer [(Identifier,HWType)] a }
  deriving (Functor, Monad, MonadWriter [(Identifier,HWType)])
