{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DBModels where

import ClassyPrelude.Conduit
import ClassyPrelude.Yesod
import Control.Monad.Reader
import Database.Persist.MongoDB (MongoContext)
import Database.Persist.TH (mkPersist, mkPersistSettings, persistLowerCase, share)
import Language.Haskell.TH.Syntax

let mongoSettings = mkPersistSettings (ConT ''MongoContext)
 in share
      [mkPersist mongoSettings]
      [persistLowerCase|
EncKey
    fsId String
    cipherKey ByteString
    cipherIv ByteString
    deriving Show
|]
