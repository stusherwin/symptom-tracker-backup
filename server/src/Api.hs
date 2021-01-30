{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Api where

import Data.Text (Text)
import Servant

type Api =
       AppApi
  :<|> Raw

type AppApi =
     "api" :> "data" :> Get '[JSON] Int

fullApi :: Proxy Api
fullApi = Proxy

appApi :: Proxy AppApi
appApi = Proxy