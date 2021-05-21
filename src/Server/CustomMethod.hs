{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.CustomMethod where

import Data.Aeson (FromJSON, ToJSON)
import Data.Loc (Loc)
import Data.Text (Text)
import GHC.Generics (Generic)
import Render
import Syntax.Predicate (Origin, PO, Spec)
import Data.Loc.Range

--------------------------------------------------------------------------------

-- | Response
data ResKind
  = ResDisplay Int [Block]
  | ResUpdateSpecs [(Int, Text, Text, Loc)]
  | ResConsoleLog Text
  deriving (Eq, Generic)

instance ToJSON ResKind

instance Show ResKind where
  show (ResDisplay _ _) = "Display"
  show (ResUpdateSpecs _) = "UpdateSpecs"
  show (ResConsoleLog x) = "ConsoleLog " <> show x

data Response
  = Res FilePath [ResKind]
  | CannotDecodeRequest String
  | NotLoaded
  deriving (Generic)

instance ToJSON Response

instance Show Response where
  show (Res _path kinds) = show kinds
  show (CannotDecodeRequest s) = "CannotDecodeRequest " <> s
  show NotLoaded = "NotLoaded"

--------------------------------------------------------------------------------

-- | Request
data ReqKind
  = ReqInspect Range
  | ReqRefine Range
  | ReqDebug
  deriving (Generic)

instance FromJSON ReqKind

instance Show ReqKind where
  show (ReqInspect range) = "Inspect " <> show range
  show (ReqRefine range) = "Refine " <> show range
  show ReqDebug = "Debug"

data Request = Req FilePath ReqKind
  deriving (Generic)

instance FromJSON Request

instance Show Request where
  show (Req _path kind) = show kind

--------------------------------------------------------------------------------

instance ToJSON Origin

instance ToJSON PO

instance ToJSON Spec
