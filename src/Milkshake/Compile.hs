{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Milkshake.Compile
  ( run
  , compile
  ) where

import           Control.Applicative          ((<|>))
import           Control.Monad                (forM_, msum, unless, when)
import           Data.Aeson                   hiding (Success)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy.Char8   as C8
import           Data.Char                    (toLower)
import qualified Data.HashMap.Strict          as HM
import           Data.List                    (intercalate, intersect,
                                               isSuffixOf)
import qualified Data.Map.Lazy                as M
import qualified Data.Set                     as S
import           Data.String                  (fromString)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as LT
import           Milkshake.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Options.Applicative
import           System.Directory             (createDirectoryIfMissing,
                                               doesDirectoryExist,
                                               doesFileExist, listDirectory,
                                               removeDirectoryRecursive)
import           System.Exit                  (exitFailure)
import           System.FilePath              (dropExtensions, joinPath,
                                               replaceExtensions,
                                               splitDirectories, takeBaseName,
                                               takeExtension, takeExtensions,
                                               takeFileName, (-<.>), (</>))
import           Text.Pandoc
import           Text.Pandoc.Readers.Markdown
import           Text.Pandoc.Shared
import           Text.Pretty.Simple           (pPrint, pShow)

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
  let version = "milkshake v0.02"
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
    file <- B.readFile pth
    compilePage prefix page{pageSourcePath=InMemory file
                           ,pageMeta=M.insert "source" pth $ pageMeta page
                           }
  RemotePath pth -> do
    putStrLn $ "Reading remote file " ++ pth
    mngr <- newManager tlsManagerSettings
    let req = fromString pth
    body <- (C8.toStrict . responseBody) <$> httpLbs req mngr
    compilePage prefix page{pageSourcePath=InMemory body
                           ,pageMeta=M.insert "source" pth $ pageMeta page
                           }
  InMemory file -> case pageOp page of
    PageOpCopy   -> do
      let dest = prefix </> pageName page
      putStrLn $ "  Copying to file " ++ dest
      B.writeFile dest file
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
      (Pandoc (Meta meta) blocks, ws) <-
        case reader $ C8.unpack $ C8.fromStrict file of
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
          putStrLn $ "  Failed: " ++ (LT.unpack $ pShow er)
          exitFailure
        Right html -> do
          writeFile dest html
          putStrLn "  Done."

-- Check to see if the pathA's last comp is also pathB's first comp, if so
-- return the portion that is different, in order.
separateFrom :: Eq a => [a] -> [a] -> [a]
separateFrom pathA pathB =
  if nter `isSuffixOf` pathA
  then take (length pathA - length nter) pathA
  else pathA
  where nter = pathA `intersect` pathB

compile
  :: FilePath
  -- ^ The directory in which the operation is happening
  -> Dir
  -- ^ The immediate/relative next directory
  -> IO ()
compile prePrefix page@(DirCopiedFrom dir excludes opmap) = do
  -- If prefix /= prePrefix it's most likely that the user has already copied
  -- from a local dir and is now adding some custom stuff to it, and we don't
  -- want to create another nested dir.
  let prefix = joinPath $
        separateFrom (splitDirectories prePrefix) (splitDirectories dir)
  putStrLn $ LT.unpack $ LT.unlines [ "compile"
                                    , "  " <> pShow prePrefix
                                    , "  " <> pShow prefix
                                    , "  " <> pShow page
                                    ]
  isDir <- doesDirectoryExist dir
  if isDir
    then do
      files <- listDirectory dir
      forM_ files $ \file -> when (not $ takeFileName file `elem` excludes) $ do
        let from      = dir </> file
            prefixDir = prefix </> dir
            dest      = prefixDir </> file
        doesDirectoryExist from >>= \case
          True -> do
            createDirectoryIfMissing True dest
            compile prefixDir (DirCopiedFrom from excludes opmap)
          False -> do
            createDirectoryIfMissing True prefixDir
            let mpage = msum $ flip map opmap $ \(Operation opFromExt opToExt op) -> do
                  let getExt = takeExtensions . (replaceExtensions "blah.txt") . map toLower
                      oldExt = getExt opFromExt
                      newExt = getExt opToExt
                  if oldExt == getExt (takeExtensions dest)
                  then return Page{ pageName = takeBaseName dest -<.> newExt
                                  , pageSourcePath = LocalPath from
                                  , pageMeta       = mempty
                                  , pageOp         = op
                                  }
                  else fail ""
            case mpage of
              Nothing -> do
                putStrLn $ "  no bulk operation in dir copied from " ++ from ++ " to " ++ dest
                putStrLn "  simply copying the file."
                compilePage prefixDir
                  Page{ pageName       = (dropExtensions $ takeFileName dest) -<.> takeExtensions from
                      , pageSourcePath = LocalPath from
                      , pageMeta       = mempty
                      , pageOp         = PageOpCopy
                      }
              Just pageToCompile -> do
                compilePage prefixDir pageToCompile

    else putStrLn ("  " ++ show dir ++ "is not a directory") >> exitFailure
compile prefix site = do
  putStrLn $ LT.unpack $ LT.unlines ["compile", "  " <> pShow prefix, "  " <> pShow site]
  let dir       = dirName site
      prefixDir = prefix </> dir
  createDirectoryIfMissing True prefixDir
  mapM_ (compilePage prefixDir) $ dirFiles site
  mapM_ (compile prefixDir)     $ dirSubdirs site
