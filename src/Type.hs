{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Type where

import           Data.Aeson
import           Data.Aeson.Types               ( prependFailure
                                                , typeMismatch
                                                )
import qualified Data.Loc                      as L
import           GHC.Generics


import           Syntax.Parser.Util             ( )

--------------------------------------------------------------------------------
-- | Instances of ToJSON

-- instance ToJSON Pos where
--   toJSON (Pos filepath line column offset) = object
--     [ "filepath" .= filepath
--     , "line" .= line
--     , "column" .= column
--     , "offset" .= offset
--     ]

--   toEncoding (Pos filepath line column offset) =
--     pairs
--       $  "filepath"
--       .= filepath
--       <> "line"
--       .= line
--       <> "column"
--       .= column
--       <> "offset"
--       .= offset

-- instance ToJSON Loc where
--   toJSON NoLoc           = object ["tag" .= ("NoLoc" :: String)]
--   toJSON (Loc start end) = object
--     [ "tag" .= ("Loc" :: String)
--     , "contents" .= object ["start" .= start, "end" .= end]
--     ]

--------------------------------------------------------------------------------
-- | Instances of FromJSON

-- instance FromJSON Loc where
--   parseJSON (Object v) =
--     Pos <$> v .: "filepath" <*> v .: "line" <*> v .: "column" <*> v .: "offset"
--   parseJSON invalid =
--     prependFailure "parsing Loc failed, " (typeMismatch "Object" invalid)

--------------------------------------------------------------------------------
-- | Pos

data Pos = Pos !FilePath
               {-# UNPACK #-} !Int
               {-# UNPACK #-} !Int
               {-# UNPACK #-} !Int deriving Generic

instance FromJSON Pos where
instance ToJSON Pos where

toPos :: Pos -> L.Pos
toPos (Pos w x y z) = L.Pos w x y z

fromPos :: L.Pos -> Pos
fromPos (L.Pos w x y z) = Pos w x y z

instance ToJSON L.Pos where
  toJSON = toJSON . fromPos
instance FromJSON L.Pos where
  parseJSON = fmap toPos . parseJSON

--------------------------------------------------------------------------------
-- | Loc

data Loc =  NoLoc
          |  Loc  {-# UNPACK #-} !Pos
                 {-# UNPACK #-} !Pos deriving Generic

instance FromJSON Loc where
instance ToJSON Loc where

toLoc :: Loc -> L.Loc
toLoc NoLoc     = L.NoLoc
toLoc (Loc x y) = L.Loc (toPos x) (toPos y)

fromLoc :: L.Loc -> Loc
fromLoc L.NoLoc     = NoLoc
fromLoc (L.Loc x y) = Loc (fromPos x) (fromPos y)

instance ToJSON L.Loc where
  toJSON = toJSON . fromLoc
instance FromJSON L.Loc where
  parseJSON = fmap toLoc . parseJSON
