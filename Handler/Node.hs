module Handler.Node where

import Import

getNodesR :: Handler Html
getNodesR = do
  -- nodes <- 
  defaultLayout $ do
    $(widgetFile "node_list")

nodeForm :: Form (Text, Text, NodeType, Bool, Bool)
