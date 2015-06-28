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
  , Balance(..)
  , BalanceId
  , Budget(..)
  , BudgetId
  , Transaction(..)
  , TransactionId
  , migrateAll
  )
  where

import Data.Time (Day, UTCTime)
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
  deadline Day
  created UTCTime default=CURRENT_TIME
  UniqueDeadline deadline
  deriving Show

MoneyNode
  nodeType MoneyNodeType -- 種別
  slug Text -- 一意なアルファベットID
  description Text -- 説明文
  created UTCTime default=CURRENT_TIME
  UniqueSlug slug
  deriving Show

Balance -- 残高
  node MoneyNode -- ノード
  balance Int -- 残高の値
  date Day -- 日付
  created UTCTime default=CURRENT_TIME
  UniqueNodeDate node date

Budget -- 予算
  node MoneyNode
  budget Int
  end AccountClosing
  created UTCTime default=CURRENT_TIME
  UniqueBudget node end

Transaction -- 取引
  value Int -- 資金
  credit MoneyNode -- お金の出処
  debit MoneyNode -- お金の行先
  date Day -- 決済日
  created UTCTime default=CURRENT_TIME
  deriving Show
|]
