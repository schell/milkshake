{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Milkshake.Types
  ( SourcePath(..)
  , PageOp(..)
  , Page(..)
  , Dir(..)
  , DirLocation(..)
  , BulkOperation(..)
  , printExampleSite
  , decodeDirFromFile
  ) where

import           Control.Applicative        ((<|>))
import           Data.Aeson
import           Data.Aeson.Types           (typeMismatch)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Map                   (Map)
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Yaml                  (ParseException, decodeFileEither)
import           Data.Yaml.Pretty
import           GHC.Generics


--------------------------------------------------------------------------------
-- SourcePath
--------------------------------------------------------------------------------


data SourcePath = LocalPath FilePath
                | InMemory B.ByteString
                deriving (Eq, Generic)


instance Show SourcePath where
  show (LocalPath fp) = "LocalPath " ++ show fp
  show (InMemory _)   = "InMemory _"


instance ToJSON SourcePath where
  toJSON (LocalPath path) = object ["localPath" .= path]
  toJSON (InMemory file)  = object ["inMemory" .= B.unpack file]


instance FromJSON SourcePath where
  parseJSON (Object v) = local <|> mem
    where local  = LocalPath <$> v .: "localPath"
          mem    = InMemory . B.pack <$> v .: "inMemory"
  parseJSON e = fail ("Could not parse " ++ show e)


--------------------------------------------------------------------------------
-- PageOp
--------------------------------------------------------------------------------
data PageOp = PageOpCopy
            | PageOpPandoc
            deriving (Show, Eq, Generic)


instance ToJSON PageOp where
  toEncoding = genericToEncoding defaultOptions


instance FromJSON PageOp


--------------------------------------------------------------------------------
-- Page
--------------------------------------------------------------------------------


-- TODO: Use NonEmpty instead of [PageOp]


data Page = Page { pageName       :: String
                 , pageSourcePath :: SourcePath
                 , pageMeta       :: Map String String
                 , pageDerivation :: [Text]
                 , pageOp         :: PageOp
                 } deriving (Show, Eq, Generic)


instance ToJSON Page where
  toEncoding = genericToEncoding defaultOptions


instance FromJSON Page where
  parseJSON (Object v) =
    Page
    <$> v .: "pageName"
    <*> v .: "pageSourcePath"
    <*> (v .: "pageMeta" <|> return mempty)
    <*> (v .: "pageDerivation" <|> return mempty)
    <*> (v .: "pageOp" <|> return PageOpPandoc)
  parseJSON e = fail $ "Could not decode " ++ show e


--------------------------------------------------------------------------------
-- Dir
--------------------------------------------------------------------------------


data BulkOperation = Operation { operationExtFrom :: String
                               , operationExtTo   :: String
                               , operationPageOp  :: PageOp
                               } deriving (Show, Eq, Generic)


instance ToJSON BulkOperation where
  toEncoding = genericToEncoding defaultOptions


instance FromJSON BulkOperation where
  parseJSON (Object v) =
    Operation
    <$> v .: "operationExtFrom"
    <*> v .: "operationExtTo"
    <*> v .: "operationPageOp"
  parseJSON e = fail $ "Could not decode " ++ show e


data DirLocation
  = LocalDir FilePath
  | ExternalZip String
  deriving (Eq, Show)


instance FromJSON DirLocation where
  parseJSON (String s) =
    return
      $ LocalDir
      $ T.unpack s
  parseJSON (Object v) =
    local <|> external
    where
      local =
        LocalDir
        <$> (v .: "localDir")
      external =
        ExternalZip
        <$> (v .: "externalZippedDir")
  parseJSON v = typeMismatch "DirLocation" v


instance ToJSON DirLocation where
  toJSON (LocalDir s)    = object ["localDir" .= T.pack s]
  toJSON (ExternalZip s) = object ["externalZippedDir" .= T.pack s]


instance IsString DirLocation where
  fromString = LocalDir


data Dir = Dir { dirName    :: String
               , dirFiles   :: [Page]
               , dirSubdirs :: [Dir]
               }
         | DirCopiedFrom { dirCopiedFrom :: DirLocation
                         , dirExcludes   :: [String]
                         , dirOpMap      :: [BulkOperation]
                         }
         deriving (Show, Eq, Generic)


instance ToJSON Dir where
  toJSON (DirCopiedFrom dir ex opmap) = object [ "dirCopiedFrom" .= dir
                                               , "dirExcludes"   .= ex
                                               , "dirOpMap"      .= opmap
                                               ]
  toJSON (Dir name files subdirs) =
    object [ "dirName" .= name
           , "dirFiles" .= files
           , "dirSubdirs" .= subdirs
           ]


instance FromJSON Dir where
  parseJSON (Object v) = copied <|> explicit
    where copied   = DirCopiedFrom <$> v .: "dirCopiedFrom"
                                   <*> (v .: "dirExcludes" <|> return mempty)
                                   <*> (v .: "dirOpMap" <|> return mempty)
          explicit = Dir <$> v .: "dirName"
                         <*> (v .: "dirFiles" <|> return mempty)
                         <*> (v .: "dirSubdirs" <|> return mempty)
  parseJSON e = fail $ "Could not decode " ++ show e


--------------------------------------------------------------------------------
-- An Example Site
--------------------------------------------------------------------------------


exampleSite :: Dir
exampleSite = Dir "root" [index] [img,css,singleArticle,articles,zipd]
  where index = Page "index.html" (LocalPath "content/index.md") mempty mempty PageOpPandoc
        img   = DirCopiedFrom "img" [".DS_Store"] []
        css   = DirCopiedFrom "css" [".DS_Store"] []
        zipd  =
          DirCopiedFrom
            (ExternalZip "https://zyghost-guides.s3-us-west-2.amazonaws.com/intro-to-rust-web.zip")
            [".DS_Store"]
            []
        articles = DirCopiedFrom "articles" [".DS_Store"]
          [Operation { operationExtFrom = "md"
                     , operationExtTo = "html"
                     , operationPageOp = PageOpPandoc
                     }
          ]


singleArticle :: Dir
singleArticle =
  Dir
    "single-article"
    [ Page
        "index.html"
        (LocalPath "Part-One.lhs")
        mempty
        derive
        PageOpPandoc
    ]
    []
  where derive =
          pure
          $ T.unwords
            [ "wget -o Part-One.lhs"
            , "https://raw.githubusercontent.com/schell/odin/8c7296fbd95bd92a40aedb06938c76174fe3e699/src/Part-One.lhs"
            ]


printExampleSite :: IO ()
printExampleSite =
  C8.putStrLn $ C8.fromStrict $ encodePretty defConfig exampleSite


decodeDirFromFile :: FilePath -> IO (Either ParseException Dir)
decodeDirFromFile = decodeFileEither
