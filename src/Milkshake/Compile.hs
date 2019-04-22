{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Milkshake.Compile
  ( run
  , compile
  , milkshakeCommand
  ) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (forM_, join, msum, unless, when)
import           Data.Aeson                 hiding (Success)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Char                  (toLower)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (intercalate, intersect, isSuffixOf)
import qualified Data.Map.Lazy              as M
import           Data.String                (fromString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy             as LT
import           Network.HTTP.Client        (httpLbs, newManager, responseBody)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Options.Applicative        (InfoMod, Parser, ParserInfo,
                                             execParser, fullDesc, header, help,
                                             helper, info, long, metavar,
                                             progDesc, short, strArgument,
                                             switch)
import           System.Directory           (createDirectoryIfMissing,
                                             doesDirectoryExist, doesFileExist,
                                             listDirectory,
                                             removeDirectoryRecursive)
import           System.Exit                (exitFailure)
import           System.FilePath            (dropExtensions, joinPath,
                                             replaceExtensions,
                                             splitDirectories, takeBaseName,
                                             takeExtension, takeExtensions,
                                             takeFileName, (-<.>), (</>))
import           Text.Pandoc                (Extension (..), Inline (..),
                                             Meta (..), MetaValue (..),
                                             Pandoc (..), PandocError,
                                             WriterOptions (..),
                                             compileTemplate, def,
                                             extensionsFromList, lookupMeta,
                                             pandocExtensions, readMarkdown,
                                             readerExtensions, renderTemplate,
                                             runPure, writeHtml5String,
                                             writerExtensions)
import           Text.Pandoc.Highlighting   (zenburn)
import           Text.Pandoc.Shared         (stringify)
import           Text.Pretty.Simple         (pShow)

import           Milkshake.Types


data Configuration
  = Configuration FilePath Bool
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


milkshakeIO :: Configuration -> IO ()
milkshakeIO ShowExample = printExampleSite
milkshakeIO (Configuration smfile shouldClean) =
  decodeDirFromFile smfile >>= \case
    Left er  -> print er >> exitFailure
    Right sm -> do
      rootExists <- doesDirectoryExist $ dirName sm
      when (shouldClean && rootExists) $ do
        putStrLn $ "Cleaning (removing) root directory " ++ show (dirName sm)
        removeDirectoryRecursive (dirName sm)
      compile "" sm


milkshakeCommand :: ParserInfo (IO ())
milkshakeCommand = info (helper <*> cmd) cmdInfo
  where
    cmdInfo :: InfoMod (IO ())
    cmdInfo =
      fullDesc
      <> progDesc "A static site compiler"
      <> header "milkshake v0.03"
    cmd = milkshakeIO <$> optsParser


run :: IO ()
run = join $ execParser milkshakeCommand


--------------------------------------------------------------------------------
-- Pandoc
--------------------------------------------------------------------------------


extExtensions :: String -> [Extension]
extExtensions ".md" = filter (/= Ext_literate_haskell) allMyPandocExtensions
extExtensions _     = allMyPandocExtensions


allMyPandocExtensions :: [Extension]
allMyPandocExtensions = [ Ext_link_attributes
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


renderPageToVars :: RenderablePage -> Either PandocError (HM.HashMap Text Value)
renderPageToVars (RenderablePage pandoc@(Pandoc meta _)) = do
  let opts :: WriterOptions
      opts = def{ writerExtensions = extensionsFromList allMyPandocExtensions <> pandocExtensions
                  , writerHighlightStyle = Just zenburn
                  --, writerHtml5 = True
                  --, writerStandalone = False
                  }
  body <- runPure
    $ writeHtml5String
        opts
        pandoc
  toc <- runPure
    $ writeHtml5String
        opts { writerTemplate = Just "$toc$"
             --,writerStandalone = True
             , writerTableOfContents = True
             }
        pandoc
  let vars = HM.fromList [ ("body", String body)
                         , ("toc", String toc)
                         ]

      accum acc k v = HM.insert (T.pack k) (stringifyHTML v) acc
      metaVars = M.foldlWithKey accum HM.empty $ unMeta meta
      allVars = metaVars <> vars
  return allVars


newtype RenderablePage = RenderablePage Pandoc


instance ToJSON RenderablePage where
  toJSON page = case renderPageToVars page of
    Right vs -> Object vs
    Left err ->
      Object
      $ HM.fromList
        [ ( "body"
          , String
            $ T.unwords
              [ "Got pandoc error: "
              , T.pack $ show err
              ]
          )
        ]


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
    body <- C8.toStrict . responseBody <$> httpLbs req mngr
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
      let reader = runPure . readMarkdown myOpts
          ext    = maybe ".md" takeExtension $ M.lookup "source" $ pageMeta page
          myOpts = def{ readerExtensions =
                             extensionsFromList (extExtensions ext)
                          <> pandocExtensions
                          <> extensionsFromList [Ext_smart]
                      }
      putStrLn $ "  Using extensions for " ++ show ext
      Pandoc (Meta meta) blocks <-
        case reader $ T.decodeUtf8 file of
          Left er -> putStrLn "Milkshake error: " >> print er >> exitFailure
          Right p -> return p

      --unless (null ws) $ putStrLn $ unlines $ "WARNINGS: ":ws

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
          putStrLn $ "  Failed: " ++ LT.unpack (pShow er)
          exitFailure
        Right html -> do
          writeFile dest html
          putStrLn "  Done."


-- | Check to see if the pathA's last comp is also pathB's first comp, if so
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
      forM_ files $ \file ->
        unless (takeFileName file `elem` excludes) $ do
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
                    let getExt = takeExtensions
                                 . replaceExtensions "blah.txt"
                                 . map toLower
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
                    Page{ pageName       = dropExtensions (takeFileName dest) -<.> takeExtensions from
                        , pageSourcePath = LocalPath from
                        , pageMeta       = mempty
                        , pageOp         = PageOpCopy
                        }
                Just pageToCompile -> compilePage prefixDir pageToCompile
    else putStrLn ("  " ++ show dir ++ "is not a directory") >> exitFailure

compile prefix site = do
  putStrLn $ LT.unpack $ LT.unlines ["compile", "  " <> pShow prefix, "  " <> pShow site]
  let dir       = dirName site
      prefixDir = prefix </> dir
  createDirectoryIfMissing True prefixDir
  mapM_ (compilePage prefixDir) $ dirFiles site
  mapM_ (compile prefixDir)     $ dirSubdirs site
