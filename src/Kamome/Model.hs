{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module
  Kamome.Model
  ( AccountClosing(..)
  , AccountClosingId
  , MoneyNode(..)
  , MoneyNodeId
  , Payment(..)
  , PaymentId
  , migrateAll
  )
  where

import Data.Time (UTCTime)
import Data.Text (Text)
import Database.Persist.TH
import Kamome.Model.Types

{-
開発メモ:
# 欲しい機能:
## v1
- 月当たり予算額を決定できる.
- 次の決算を設定できる.
- 次の決算までの支払いを事前に設定できる.
- 未確定の支払いを決済日に確定できる.
- 1日分の余剰チケット額を計算できる.
-}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
AccountClosing
  deadline UTCTime
  budget Int
  deriving Show

MoneyNode
  label Text
  nodeType MoneyNodeType
  UniqueLabel label
  deriving Show

Payment
  label Text
  value Int
  status PaymentStatus
  credit MoneyNode -- お金の出処
  debit MoneyNode -- お金の行先
  created UTCTime default=CURRENT_TIME
  payed UTCTime
  deriving Show
|]
