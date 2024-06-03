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
  -- display stuff at the panel 
  = ResDisplay Int [Section]
  -- mark Specs (holes) in the editor
  | ResUpdateSpecs [(Int, Text, Text, Range)]
  -- mark Proof Obligations in the editor 
  | ResMarkPOs [Range]
  -- perform subsititution in expressions in the panel
  | ResSubstitute Int Inlines
  -- emit console.log for debugging
  | ResConsoleLog Text
  deriving (Eq, Generic)

instance ToJSON ResKind

instance Pretty ResKind where
  pretty (ResDisplay _ _   ) = "Display"
  pretty (ResUpdateSpecs _ ) = "UpdateSpecs"
  pretty (ResMarkPOs     _ ) = "ResMarkPOs"
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
  -- new
  | ReqHelloWorld Range
  | ReqReload
  | ReqRefine2 Range Text
  | ReqInsertProofTemplate Range Text
  deriving Generic

instance FromJSON ReqKind

instance Show ReqKind where
  show (ReqInspect      range) = "Inspect " <> show (ShortRange range)
  show (ReqRefine       range) = "Refine " <> show (ShortRange range)
  show (ReqInsertAnchor hash ) = "InsertAnchor " <> show hash
  show (ReqSubstitute   i    ) = "Substitute " <> show i
  show ReqDebug                = "Debug"
  show (ReqHelloWorld   range) = "HelloWorld " <> show (ShortRange range)
  show ReqReload               = "Reload"
  show (ReqRefine2 range text) = "Refine2 " <> show (ShortRange range)
                                  <>    " " <> show text
  show (ReqInsertProofTemplate range hash)
                               = "InsertProofTemplate " <> show (ShortRange range)
                                  <>                " " <> show hash

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
