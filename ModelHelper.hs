module ModelHelper where

import Prelude
import Yesod

data NodeType = Income | Asset | Liability | Cost
              deriving (Show, Read, Eq, Ord, Enum, Bounded)

derivePersistField "NodeType"
