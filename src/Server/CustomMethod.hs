{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.CustomMethod where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Render
import GCL.Predicate (Origin, PO, Spec)
import Data.Loc.Range
import Pretty

--------------------------------------------------------------------------------

-- | Response
data ResKind
  = ResDisplay Int [Block]
  | ResUpdateSpecs [(Int, Text, Text, Range)]
  | ResConsoleLog Text
  deriving (Eq, Generic)

instance ToJSON ResKind

instance Pretty ResKind where
  pretty (ResDisplay _ _) = "Display"
  pretty (ResUpdateSpecs _) = "UpdateSpecs"
  pretty (ResConsoleLog x) = "ConsoleLog " <> pretty x

instance Show ResKind where
  show = toString 

data Response
  = Res FilePath [ResKind]
  | CannotDecodeRequest String
  deriving (Generic)

instance ToJSON Response

instance Show Response where
  show (Res _path kinds) = show kinds
  show (CannotDecodeRequest s) = "CannotDecodeRequest " <> s

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
