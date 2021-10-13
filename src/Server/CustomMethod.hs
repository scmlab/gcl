{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.CustomMethod where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Loc.Range
import           Data.Text                      ( Text )
import           GCL.Predicate                  ( InfMode
                                                , Origin
                                                , PO
                                                , Spec
                                                )
import           GHC.Generics                   ( Generic )
import           Pretty
import           Render

--------------------------------------------------------------------------------

-- | Response
data ResKind
  = ResDisplay Int [Section]
  | ResUpdateSpecs [(Int, Text, Text, Range)]
  | ResSubstitute Int Inlines
  | ResConsoleLog Text
  deriving (Eq, Generic)

instance ToJSON ResKind

instance Pretty ResKind where
  pretty (ResDisplay _ _   ) = "Display"
  pretty (ResUpdateSpecs _ ) = "UpdateSpecs"
  pretty (ResSubstitute i x) = "Substitute " <> pretty i <> " : " <> pretty x
  pretty (ResConsoleLog x  ) = "ConsoleLog " <> pretty x

instance Show ResKind where
  show = toString

data Response
  = Res FilePath [ResKind]
  | CannotDecodeRequest String
  deriving (Generic)

instance ToJSON Response

instance Show Response where
  show (Res _path kinds      ) = show kinds
  show (CannotDecodeRequest s) = "CannotDecodeRequest " <> s

--------------------------------------------------------------------------------

-- | Requests from the client 
data ReqKind
  = ReqInspect Range
  | ReqRefine Range
  | ReqInsertAnchor Text
  | ReqSubstitute Int
  | ReqDebug
  deriving (Generic)

instance FromJSON ReqKind

instance Show ReqKind where
  show (ReqInspect      range) = "Inspect " <> show range
  show (ReqRefine       range) = "Refine " <> show range
  show (ReqInsertAnchor hash ) = "InsertAnchor " <> show hash
  show (ReqSubstitute   i    ) = "Substitute " <> show i
  show ReqDebug                = "Debug"

data Request = Req FilePath ReqKind
  deriving Generic

instance FromJSON Request

instance Show Request where
  show (Req _path kind) = show kind

--------------------------------------------------------------------------------

instance ToJSON Origin
instance ToJSON InfMode

instance ToJSON PO

instance ToJSON Spec
