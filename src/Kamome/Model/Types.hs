module
  Kamome.Model.Types
  ( MoneyNodeType(..)
  , PaymentStatus(..)
  )
  where

import Database.Persist.Class (PersistField, toPersistValue, fromPersistValue)
import Database.Persist.Sql (PersistFieldSql, sqlType)
import Database.Persist.Types (PersistValue(..), SqlType(..))
import qualified Data.Text as T

-- | Kind of money node.
data MoneyNodeType
  = Asset -- ^ 資産
  | Liability -- ^ 負債
  | Revenue -- ^ 収益
  | Expense -- ^ 費用
  deriving (Show, Enum, Eq, Bounded)

-- | Status of payment.
data PaymentStatus
  = Fixed -- ^ Already fixed.
  | ToBeChecked -- ^ Not fixed and should be checked later.
  deriving (Show, Enum, Eq, Bounded)

-- |
-- Internal function of enumFromPersistValue.
-- You should use minb and maxb otherwise you'll suffer from minBound/maxBound type error.
enumFromPersistValueBetween :: (Bounded a, Enum a) => a -> a -> PersistValue -> Either T.Text a
enumFromPersistValueBetween minb maxb x = case x of
    PersistInt64 i | i >= mini && i <= maxi -> Right $ toEnum $ fromIntegral i
                   | otherwise -> Left $ T.pack $ "Enum out of range, received: " ++ show i
    PersistDouble i | j >= mini && j <= maxi -> Right $ toEnum $ fromIntegral j -- oracle
                    | otherwise -> Left $ T.pack $ "Enum out of range, received: " ++ show i
      where
        j = truncate i
    _ -> Left $ T.pack $ "Enum Expected Integer, received: " ++ show x
    where
      mini = fromIntegral $ fromEnum minb
      maxi = fromIntegral $ fromEnum maxb

-- |
-- Make Enum from PersistValue.
enumFromPersistValue :: (Bounded a, Enum a) => PersistValue -> Either T.Text a
enumFromPersistValue = enumFromPersistValueBetween minBound maxBound

instance PersistField MoneyNodeType where
  toPersistValue = PersistInt64 . fromIntegral . fromEnum
  fromPersistValue = enumFromPersistValue

instance PersistFieldSql MoneyNodeType where
  sqlType _ = SqlInt32

instance PersistField PaymentStatus where
  toPersistValue = PersistInt64 . fromIntegral . fromEnum
  fromPersistValue = enumFromPersistValue

instance PersistFieldSql PaymentStatus where
  sqlType _ = SqlInt32

