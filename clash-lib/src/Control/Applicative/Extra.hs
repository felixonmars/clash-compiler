{-|
  Copyright   :  (C) 2020, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Control.Applicative.Extra
  ( orEmpty
  ) where

import Control.Applicative (Alternative, empty)

orEmpty :: Alternative f => Bool -> a -> f a
orEmpty b a = if b then pure a else empty
