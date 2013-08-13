module Handler.Edge where

import Import

getEdgesR :: Handler Html
getEdgesR = do
  -- edges <-
  defaultLayout $ do
    $(widgetFile "edge_list")
