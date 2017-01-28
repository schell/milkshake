{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Milkshake where

import Development.Shake
import Development.Shake.FilePath
import qualified Data.Text as T
import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as PrettyJSON
import qualified Data.Yaml.Pretty as PrettyYAML
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Lazy as M
import           Data.Map.Lazy (Map) 
import qualified Data.Set as S
import Control.Monad
import Text.Pandoc
import Text.Pandoc.Shared
import Text.Pandoc.XML
import Text.Pandoc.Readers.Markdown
import System.Exit
import GHC.Generics hiding (Meta)
--------------------------------------------------------------------------------
-- Milkshake
--------------------------------------------------------------------------------
data MilkshakeConfig = MilkshakeConfig { mscContentDirectory :: FilePath
                                       , mscCSSDirectory     :: FilePath
                                       , mscImagesDirectory  :: FilePath
                                       , mscBuildDirectory   :: FilePath
                                       }

defaultMilkshakeConfig :: MilkshakeConfig
defaultMilkshakeConfig =
  MilkshakeConfig "content" "css" "img" "build"

defaultMilkshakeConfigIn :: FilePath -> MilkshakeConfig
defaultMilkshakeConfigIn dir =
  MilkshakeConfig content css img build
    where [content,css,img,build] = map (dir </>) ["content", "css", "img", "build"]
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
                 } deriving (Generic, Show, Eq)

newtype PageRendering = PageRendering Page

instance ToJSON PageRendering where
  toJSON (PageRendering (Page path template pandoc@(Pandoc meta _))) =
    let optsPlain  = def{ writerExtensions = S.union myPandocExtensions $
                                              writerExtensions def
                        , writerHighlight = True
                        , writerHtml5 = True
                        }
        opts = optsPlain{ writerStandalone = False
                        , writerTableOfContents = True
                        }
        bodySrc      = writeHtmlString opts pandoc
        toc = String $ T.pack $ writeHtmlString opts{writerTemplate = "$toc$"
                                                    ,writerStandalone = True
                                                    } pandoc
        vals = [ "source"   .= bodySrc
               , "toc"      .= toc
               , "from"     .= path
               , "template" .= template
               , "meta"     .= metaToObject meta
               ]
        obj = object vals
        bodyCompiled = either (const bodySrc) (`renderTemplate` obj) $
                         compileTemplate $ T.pack bodySrc
      in object $ ("body" .= bodyCompiled):vals

instance ToJSON Page where
  toJSON page = object [ "pagePath"      .= pagePath page
                       , "pageTemplate"  .= pageTemplate page
                       , "pagePandoc"    .= pagePandoc page
                       , "pageRendering" .= PageRendering page
                       ] 

renderTemplateTextWith :: ToJSON a => Text -> a -> String
renderTemplateTextWith template a =
  either (const $ T.unpack backupTemplate)
         (`renderTemplate` (toJSON a))
         $ compileTemplate template

renderPage :: Page -> String
renderPage page = renderTemplateTextWith (pageTemplate page) $ PageRendering page

metaToObject :: Meta -> Object
metaToObject meta = M.foldlWithKey accum HM.empty $ unMeta meta
  where accum acc k v = HM.insert (T.pack k) (stringifyHTML v) acc
--------------------------------------------------------------------------------
-- Caches
--------------------------------------------------------------------------------
data Sitemap = Sitemap { sitemapStaticFiles    :: [FilePath]
                         -- ^ List of all static files
                       , sitemapGeneratedFiles :: Map FilePath Page
                         -- ^ Map of all the generated files keyed by their paths
                         -- and with a value of their metadata.
                       } deriving (Generic, Show, Eq)

instance ToJSON Sitemap where
  toEncoding = genericToEncoding defaultOptions

type GetPage = FilePath -> Action Page
type GetTemplate = FilePath -> Action Text
--type GetData     = FilePath -> Action Object
type GetSitemap  = ()       -> Action Sitemap

backupTemplate :: Text
backupTemplate =
  "<html><head><title>$title$</title></head><body>$body$</body></html>"

makeGetTemplate :: Rules GetTemplate
makeGetTemplate = newCache $ \file -> doesFileExist file >>= \case
  False -> liftIO $ do
    putStrLn $ "Milkshake error: could not find theme " ++ show file
    exitFailure 
  True -> T.pack <$> readFile' file

makeGetPage :: GetTemplate -> Rules GetPage
makeGetPage getTemplate = newCache $ \file -> do
  let reader = readMarkdownWithWarnings myOpts
      myOpts = def{ readerExtensions = S.union myPandocExtensions $
                                         readerExtensions def
                  , readerSmart = True
                  }
  (pandoc@(Pandoc meta _), ws) <- (reader <$> readFile' file) >>= \case
    Left er -> liftIO $ do
      putStrLn "Milkshake error: "
      print er
      exitFailure
    Right p  -> liftIO (putStrLn "got pandoc") >> return p
  unless (null ws) $ liftIO $ putStrLn $ unlines $ "WARNINGS: ":ws

  template <- case lookupMeta "theme" meta of
    Just theme -> getTemplate $ stringify theme
    Nothing    -> return backupTemplate

  let page = Page { pagePath = file
                  , pageTemplate = template
                  , pagePandoc = pandoc
                  }
  return page

makeGetSitemap :: MilkshakeConfig -> GetPage -> Rules GetSitemap
makeGetSitemap MilkshakeConfig{..} getMarkdown = newCache $ \_ -> do
  css <- doesDirectoryExist mscCSSDirectory >>= \case
    True -> do
      css <- getDirectoryFiles "" [mscCSSDirectory </> "*.css"]
      return css
    _ -> return []

  imgs <- doesDirectoryExist mscImagesDirectory >>= \case 
    True -> do
      imgs <- getDirectoryFiles "" [mscImagesDirectory </> "*.*"]
      return imgs
    _ -> return []

  mds <- doesDirectoryExist mscContentDirectory >>= \case
    True -> do
      markdownPages <- getDirectoryFiles "" [mscContentDirectory ++ "//*.md"]
      return markdownPages
    _ -> return []
  
  let pages = map ((-<.> "html") . dropDirectory1) mds
  
  datas <- mapM getMarkdown mds                     
  return $ Sitemap (css ++ imgs) $ M.fromList $ zip pages datas  
  
--------------------------------------------------------------------------------
-- Helper Actions
--------------------------------------------------------------------------------
printPrettyJSON :: GetPage -> FilePath -> Action ()
printPrettyJSON getMarkdown file = do
  page <- getMarkdown file
  liftIO $ L8.putStrLn $ PrettyJSON.encodePretty $ toJSON page

printPrettyYAML :: GetPage -> FilePath -> Action ()
printPrettyYAML getMarkdown file = do
  page <- getMarkdown file
  let val = toJSON page
  liftIO $ B8.putStrLn $ PrettyYAML.encodePretty PrettyYAML.defConfig val
--------------------------------------------------------------------------------
-- Milkshake Main
--------------------------------------------------------------------------------
milkshake :: MilkshakeConfig -> IO ()
milkshake cfg@MilkshakeConfig{..} =
  shakeArgs shakeOptions{shakeFiles=mscBuildDirectory} $ do
    getTemplate <- makeGetTemplate
    getMarkdown <- makeGetPage getTemplate
    getSitemap  <- makeGetSitemap cfg getMarkdown

    phony "clean" $ removeFilesAfter mscBuildDirectory ["//*"]
    ----------------------------------------------------------------------------
    -- sitemap
    ----------------------------------------------------------------------------
    phony "sitemap" $ do
      sitemap <- getSitemap ()
      liftIO $ L8.putStrLn $ PrettyJSON.encodePretty $ toJSON sitemap
    ----------------------------------------------------------------------------
    -- build
    ----------------------------------------------------------------------------
    phony "build" $ do
      Sitemap{..} <- getSitemap ()
      let files = sitemapStaticFiles ++ M.keys sitemapGeneratedFiles
      need $ map (mscBuildDirectory </>) files 
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
