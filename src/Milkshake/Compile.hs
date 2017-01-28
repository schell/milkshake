{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Milkshake.Compile
  ( run
  , compile
  ) where

import           Control.Applicative          ((<|>))
import           Control.Monad                (forM_, unless, when)
import           Data.Aeson                   hiding (Success)
import qualified Data.ByteString.Lazy.Char8   as C8
import qualified Data.HashMap.Strict          as HM
import           Data.List                    (intercalate)
import qualified Data.Map.Lazy                as M
import qualified Data.Set                     as S
import           Data.String                  (fromString)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Milkshake.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Options.Applicative
import           System.Directory             (copyFile,
                                               createDirectoryIfMissing,
                                               doesDirectoryExist,
                                               doesFileExist, listDirectory,
                                               removeDirectoryRecursive)
import           System.Exit                  (exitFailure)
import           System.FilePath              ((</>))
import           Text.Pandoc
import           Text.Pandoc.Readers.Markdown
import           Text.Pandoc.Shared
import           Text.Pandoc.XML

data Configuration = Configuration { configSitemap     :: FilePath
                                   , configShouldClean :: Bool
                                   }
                   | ShowExample

optsParser :: Parser Configuration
optsParser = showExample <|>
  Configuration <$> sitemapParser <*> shouldCleanParser
  where sitemapParser = strArgument (  help "Site map yaml file"
                                    <> metavar "FILE"
                                    )
        shouldCleanParser = switch  (  help "Whether or not to clean first"
                                    <> long "clean"
                                    <> short 'c'
                                    )
        showExample = ShowExample <$
                        switch (  help "Show example site yaml markup"
                               <> long "example"
                               <> short 'e'
                               )

run :: [String] -> IO ()
run args = do
  let version = "milkshake v0.01"
      opts = info (helper <*> optsParser) (  fullDesc
                                          <> progDesc "A static site compiler"
                                          <> header version
                                          )
  case execParserPure defaultPrefs opts args of
    Failure er1             -> putStrLn $ fst $ renderFailure er1 "milkshake"
    CompletionInvoked compl -> execCompletion compl "milkshake" >>= putStrLn
    Success ShowExample     -> printExampleSite
    Success cfg             -> decodeDirFromFile (configSitemap cfg) >>= \case
      Left er  -> print er >> exitFailure
      Right sm -> do
        rootExists <- doesDirectoryExist $ dirName sm
        when (configShouldClean cfg && rootExists) $ do
          putStrLn $ "Cleaning (removing) root directory " ++ show (dirName sm)
          removeDirectoryRecursive (dirName sm)
        compile "" sm
--------------------------------------------------------------------------------
-- Pandoc
--------------------------------------------------------------------------------
myPandocExtensions :: S.Set Extension
myPandocExtensions =
  S.fromList [ Ext_link_attributes
             , Ext_mmd_link_attributes
             , Ext_literate_haskell
             ]

stringifyHTML :: MetaValue -> Value
stringifyHTML = String . T.pack . escapeStringForXML . stringify

inlines :: String -> MetaValue
inlines = MetaInlines . intercalate [Space] . map ((:[]) . Str) . words
--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------
newtype RenderablePage = RenderablePage Pandoc

instance ToJSON RenderablePage where
  toJSON (RenderablePage pandoc@(Pandoc meta _)) =
    let optsPlain  = def{ writerExtensions = S.union myPandocExtensions $
                                               writerExtensions def
                        , writerHighlight = True
                        , writerHtml5 = True
                        }
        opts = optsPlain{ writerStandalone = False
                        , writerTableOfContents = True
                        }
        body = String $ T.pack $ writeHtmlString opts pandoc
        toc = String $ T.pack $ writeHtmlString opts{writerTemplate = "$toc$"
                                                    ,writerStandalone = True
                                                    } pandoc
        vars = HM.fromList [ ("body", body)
                           , ("toc", toc)
                           ]

        accum acc k v = HM.insert (T.pack k) (stringifyHTML v) acc
        metaVars = M.foldlWithKey accum HM.empty $ unMeta meta
        allVars = metaVars <> vars
    in Object allVars

backupTemplate :: Text
backupTemplate =
  "<html><head><title>$title$</title></head><body>$body$</body></html>"

renderTemplateTextWith :: Text -> Value -> String
renderTemplateTextWith template val =
  either (const $ T.unpack backupTemplate)
         (`renderTemplate` val)
         $ compileTemplate template

renderPage :: RenderablePage -> Text -> String
renderPage page template =
  renderTemplateTextWith template $ toJSON page
--------------------------------------------------------------------------------
-- Compiling
--------------------------------------------------------------------------------
compilePage :: FilePath -> Page -> IO ()
compilePage prefix page = case pageSourcePath page of
  LocalPath pth  -> do
    putStrLn $ "Reading local file " ++ pth
    file <- C8.readFile pth
    compilePage prefix page{pageSourcePath=InMemory $ C8.unpack file}
  RemotePath pth -> do
    putStrLn $ "Reading remote file " ++ pth
    mngr <- newManager tlsManagerSettings
    let req = fromString pth
    body <- (C8.unpack . responseBody) <$> httpLbs req mngr
    compilePage prefix page{pageSourcePath=InMemory body}
  InMemory file -> case pageOp page of
    PageOpCopy   -> do
      let dest = prefix </> pageName page
      putStrLn $ "  Copying to file " ++ dest
      writeFile dest file
      putStrLn "  Done."
    PageOpPandoc -> do
      let dest = prefix </> pageName page
      putStrLn $ "  Pandoc'ing to file " ++ dest
      let reader = readMarkdownWithWarnings myOpts
          myOpts = def{ readerExtensions = S.union myPandocExtensions $
                                            readerExtensions def
                      , readerSmart = True
                      }
      (Pandoc (Meta meta) blocks, ws) <- case reader file of
        Left er -> putStrLn "Milkshake error: " >> print er >> exitFailure
        Right p -> return p

      unless (null ws) $ putStrLn $ unlines $ "WARNINGS: ":ws

      -- mix the page meta in with meta
      let pmeta   = inlines <$> pageMeta page
          allMeta = Meta $ meta <> pmeta
      print allMeta
      template <- case stringify <$> lookupMeta "theme" allMeta of
        Nothing    -> return backupTemplate
        Just theme -> doesFileExist theme >>= \case
          False -> do
            putStrLn $ unwords [ "  Theme file"
                               , show theme
                               , "does not exist"
                               ]
            exitFailure
          True  -> do
            putStrLn $ unwords [ "  Using theme"
                               , show theme
                               ]
            T.pack <$> readFile theme

      let rpage = RenderablePage $ Pandoc allMeta blocks
          html  = renderPage rpage template
      writeFile dest html
      putStrLn "  Done."

compile :: FilePath -> Dir -> IO ()
compile prefix (DirCopiedFrom dir) = do
  isDir <- doesDirectoryExist dir
  if isDir
    then do
      files <- listDirectory dir
      forM_ files $ \file -> do
        let from      = dir </> file
            prefixDir = prefix </> dir
            dest      = prefixDir </> file
        putStrLn $ unwords [ "Copying"
                           , from
                           , "to"
                           , dest
                           ]
        createDirectoryIfMissing True prefixDir
        copyFile from dest
    else putStrLn (show dir ++ "is not a directory") >> exitFailure
compile prefix site = do
  let dir       = dirName site
      prefixDir = prefix </> dir
  createDirectoryIfMissing True prefixDir
  mapM_ (compilePage prefixDir) $ dirFiles site
  mapM_ (compile prefixDir)     $ dirSubdirs site
