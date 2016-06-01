{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Milkshake where

import Development.Shake
import Development.Shake.FilePath
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Encode.Pretty as PrettyJSON
import qualified Data.Yaml.Pretty as PrettyYAML
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Control.Monad
import Text.Pandoc
import Text.Pandoc.Shared
import Text.Pandoc.XML
import Text.Pandoc.Readers.Markdown
import System.Exit
--------------------------------------------------------------------------------
-- Milkshake
--------------------------------------------------------------------------------
data MilkshakeConfig = MilkshakeConfig { mscContentDirectory :: FilePath
                                       , mscCSSDirectory     :: FilePath
                                       , mscImagesDirectory  :: FilePath
                                       , mscBuildDirectory   :: FilePath
                                       , mscMakeGetPageData  :: Rules GetData
                                       }

emptyGetData :: GetData
emptyGetData = const $ return HM.empty

defaultMilkshakeConfig :: MilkshakeConfig
defaultMilkshakeConfig =
  MilkshakeConfig "content" "css" "img" "build" makeGetPageData
--------------------------------------------------------------------------------
-- Pandoc
--------------------------------------------------------------------------------
myPandocExtensions :: S.Set Extension
myPandocExtensions =
  S.fromList [ Ext_link_attributes
             , Ext_mmd_link_attributes
             ]

stringifyHTML :: MetaValue -> Value
stringifyHTML = String . T.pack . escapeStringForXML . stringify
--------------------------------------------------------------------------------
-- Page
--------------------------------------------------------------------------------
data Page = Page { pagePath     :: FilePath
                 , pageTemplate :: Text
                 , pagePandoc   :: Pandoc
                 , pageData     :: Object
                 } deriving (Show, Eq)

instance ToJSON Page where
  toJSON (Page path template pandoc@(Pandoc meta _) obj) =
    let optsPlain  = def{ writerExtensions = S.union myPandocExtensions $
                                               writerExtensions def
                        , writerHighlight = True
                        , writerHtml5 = True
                        }
        opts = optsPlain{ writerStandalone = False
                        , writerTableOfContents = True
                        }
        bodyTemplate = writeHtmlString opts pandoc
        bodyString = either (const bodyTemplate)
                            (`renderTemplate` Object obj)
                            $ compileTemplate $ T.pack bodyTemplate
        body = String $ T.pack bodyString
        toc = String $ T.pack $ writeHtmlString opts{writerTemplate = "$toc$"
                                                    ,writerStandalone = True
                                                    } pandoc
        vars = HM.fromList [ ("body", body)
                           , ("toc", toc)
                           , ("source", String $ T.pack path)
                           , ("template", String template)
                           ]
                 <> obj
        accum acc k v = HM.insert (T.pack k) (stringifyHTML v) acc
        metaVars = M.foldlWithKey accum HM.empty $ unMeta meta
        allVars = metaVars <> vars
    in Object allVars

renderTemplateTextWith :: Text -> Value -> String
renderTemplateTextWith template val =
  either (const $ T.unpack backupTemplate)
         (`renderTemplate` val)
         $ compileTemplate template

renderPage :: Page -> String
renderPage page = renderTemplateTextWith (pageTemplate page) $ toJSON page
--------------------------------------------------------------------------------
-- Caches
--------------------------------------------------------------------------------
type GetMarkdown = FilePath -> Action Page
type GetTemplate = FilePath -> Action Text
type GetData     = FilePath -> Action Object

backupTemplate :: Text
backupTemplate =
  "<html><head><title>$title$</title></head><body>$body$</body></html>"

makeGetTemplate :: Rules GetTemplate
makeGetTemplate = newCache $ \file ->
  doesFileExist file >>= \case
  False -> return backupTemplate
  True -> T.pack <$> readFile' file

makeGetPageData :: Rules GetData
makeGetPageData = newCache $ \file ->
  return $ HM.singleton "source" $ String $ T.pack file

makeGetMarkdown :: GetTemplate -> GetData -> Rules GetMarkdown
makeGetMarkdown getTemplate getData = newCache $ \file -> do
  let reader = readMarkdownWithWarnings myOpts
      myOpts = def{ readerExtensions = S.union myPandocExtensions $
                                         readerExtensions def
                  , readerSmart = True
                  }
  (pandoc@(Pandoc meta _), ws) <- (reader <$> readFile' file) >>= \case
    Left er -> liftIO $ print er >> exitFailure
    Right p  -> return p
  unless (null ws) $ liftIO $ putStrLn $ unlines $ "WARNINGS: ":ws

  let templateName = maybe "default" stringify $ lookupMeta "theme" meta
  template <- getTemplate $ "templates" </> templateName  <.> "html"

  obj <- getData file
  let page = Page { pagePath = file
                  , pageTemplate = template
                  , pagePandoc = pandoc
                  , pageData = obj
                  }
  return page
--------------------------------------------------------------------------------
-- Helper Actions
--------------------------------------------------------------------------------
printPrettyJSON :: GetMarkdown -> FilePath -> Action ()
printPrettyJSON getMarkdown file = do
  page <- getMarkdown file
  liftIO $ L8.putStrLn $ PrettyJSON.encodePretty $ toJSON page

printPrettyYAML :: GetMarkdown -> FilePath -> Action ()
printPrettyYAML getMarkdown file = do
  page <- getMarkdown file
  let val = toJSON page
  liftIO $ B8.putStrLn $ PrettyYAML.encodePretty PrettyYAML.defConfig val
--------------------------------------------------------------------------------
-- Milkshake Main
--------------------------------------------------------------------------------
milkshake :: MilkshakeConfig -> IO ()
milkshake MilkshakeConfig{..} =
  shakeArgs shakeOptions{shakeFiles=mscBuildDirectory} $ do
    getData     <- mscMakeGetPageData
    getTemplate <- makeGetTemplate
    getMarkdown <- makeGetMarkdown getTemplate getData

    phony "clean" $ removeFilesAfter mscBuildDirectory ["//*"]
    ----------------------------------------------------------------------------
    -- build
    ----------------------------------------------------------------------------
    phony "build" $ do
        css <- map (mscBuildDirectory </>) <$> getDirectoryFiles "" [mscCSSDirectory </> "*.css"]
        need css

        imgs <- map (mscBuildDirectory </>) <$> getDirectoryFiles "" [mscImagesDirectory </> "*.*"]
        need imgs

        markdownPages <- getDirectoryFiles mscContentDirectory ["//*.md"]
        let pages = map ((-<.> "html") . (mscBuildDirectory </>)) markdownPages
        need pages
    ---------------------------------------------------------------------------
    -- Helpers
    ---------------------------------------------------------------------------
    phonys $ \case
      'j':'s':'o':'n':':':file -> Just $ printPrettyJSON getMarkdown file
      'y':'a':'m':'l':':':file -> Just $ printPrettyYAML getMarkdown file
      'b':'o':'d':'y':':':file -> Just $
        (toJSON <$> getMarkdown file) >>= \case
          Object obj -> liftIO $ print $ case HM.lookup "body" obj of
            Just (String str) -> str
            _ -> "Could not get body for " `T.append` T.pack file
          _ -> liftIO $ putStrLn $ "Could not get page content for " ++ file
      _ -> Nothing
    ----------------------------------------------------------------------------
    -- CSS
    ----------------------------------------------------------------------------
    mscBuildDirectory </> mscCSSDirectory </> "*.css" %> \out ->
        copyFile' (dropDirectory1 out) out
    ----------------------------------------------------------------------------
    -- images
    ----------------------------------------------------------------------------
    mscBuildDirectory </> mscImagesDirectory </> "*" %> \out ->
        copyFile' (dropDirectory1 out) out
    mscBuildDirectory <//> "*.html" %> \out -> do
      let md = mscContentDirectory </> dropDirectory1 out -<.> "md"
      page <- getMarkdown md
      writeFile' out $ renderPage page

defaultMilkshake :: IO ()
defaultMilkshake = milkshake defaultMilkshakeConfig
