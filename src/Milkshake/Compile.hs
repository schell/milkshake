{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Milkshake.Compile
  ( run
  , compile
  ) where

import           Control.Applicative           ((<|>))
import           Control.Monad                 (forM_, unless, when)
import           Data.Aeson                    hiding (Success)
import qualified Data.ByteString.Lazy.Char8    as C8
import qualified Data.HashMap.Strict           as HM
import           Data.List                     (intercalate)
import qualified Data.Map.Lazy                 as M
import qualified Data.Set                      as S
import           Data.String                   (fromString)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Milkshake.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Options.Applicative
import           System.Directory              (copyFile,
                                                createDirectoryIfMissing,
                                                doesDirectoryExist,
                                                doesFileExist, listDirectory,
                                                removeDirectoryRecursive)
import           System.Exit                   (exitFailure)
import           System.FilePath               (takeExtension, (</>))
import           Text.Highlighting.Kate.Styles (zenburn)
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
extExtensions :: String -> S.Set Extension
extExtensions ".md" = S.filter (/= Ext_literate_haskell) allMyPandocExtensions
extExtensions _     = allMyPandocExtensions

allMyPandocExtensions :: S.Set Extension
allMyPandocExtensions = S.fromList [ Ext_link_attributes
                                   , Ext_mmd_link_attributes
                                   , Ext_literate_haskell
                                   ]

stringifyHTML :: MetaValue -> Value
stringifyHTML = String . T.pack {-. escapeStringForXML-} . stringify

inlines :: String -> MetaValue
inlines = MetaInlines . intercalate [Space] . map ((:[]) . Str) . words

metaLookup :: String -> Meta -> Maybe String
metaLookup = ((stringify <$>) .) . lookupMeta

metaInsert :: String -> MetaValue -> Meta -> Meta
metaInsert key val = Meta . M.insert key val . unMeta
--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------
newtype RenderablePage = RenderablePage Pandoc

instance ToJSON RenderablePage where
  toJSON (RenderablePage pandoc@(Pandoc meta _)) =
    let opts  = def{ writerExtensions = allMyPandocExtensions <> pandocExtensions
                   , writerHighlight = True
                   , writerHtml5 = True
                  -- , writerHighlightStyle = zenburn
                   , writerStandalone = False
                   }
        body = String $ T.pack $ writeHtmlString opts pandoc
        toc = String $ T.pack $ writeHtmlString opts{writerTemplate = "$toc$"
                                                    ,writerStandalone = True
                                                    ,writerTableOfContents = True
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

renderTemplateTextWith :: ToJSON a => Text -> a -> Either String String
renderTemplateTextWith template val =
  (`renderTemplate` val) <$> compileTemplate template

renderPage :: RenderablePage -> Text -> Either String String
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
    compilePage prefix page{pageSourcePath=InMemory $ C8.unpack file
                           ,pageMeta=M.insert "source" pth $ pageMeta page
                           }
  RemotePath pth -> do
    putStrLn $ "Reading remote file " ++ pth
    mngr <- newManager tlsManagerSettings
    let req = fromString pth
    body <- (C8.unpack . responseBody) <$> httpLbs req mngr
    compilePage prefix page{pageSourcePath=InMemory body
                           ,pageMeta=M.insert "source" pth $ pageMeta page
                           }
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
          ext    = maybe ".md" takeExtension $ M.lookup "source" $ pageMeta page
          myOpts = def{ readerExtensions = extExtensions ext <> pandocExtensions
                      , readerSmart = True
                      }
      putStrLn $ "  Using extensions for " ++ show ext
      (Pandoc (Meta meta) blocks, ws) <- case reader file of
        Left er -> putStrLn "Milkshake error: " >> print er >> exitFailure
        Right p -> return p

      unless (null ws) $ putStrLn $ unlines $ "WARNINGS: ":ws

      -- mix the page meta in with meta
      let pmeta   = inlines <$> pageMeta page
          allMeta = Meta $ meta <> pmeta

      template <- case metaLookup "theme" allMeta of
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
      case renderPage rpage template of
        Left er -> do
          putStrLn $ "  Failed: " ++ show er
          exitFailure
        Right html -> do
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
  putStrLn $ "Creating directory " ++ show prefixDir
  mapM_ (compilePage prefixDir) $ dirFiles site
  mapM_ (compile prefixDir)     $ dirSubdirs site
