{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Monoid
import Data.Time
import Options.Applicative as OA
import Paths_jw
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process

data Options = Options {
    optCommand :: Command
  } deriving (Eq, Show)

data Environment = Environment {
    envDay    :: Day
  , envEditor :: Maybe String
  } deriving (Eq, Show)

data Command =
    Entry EntryOptions
  | View ViewOptions
  deriving (Eq, Show)

data EntryOptions = EntryOptions {
    entryOptDay :: Maybe Day
  } deriving (Eq, Show)

data ViewOptions = ViewOptions {
    viewOptDay :: Maybe Day
  } deriving (Eq, Show)

data Editor =
    Vim
  deriving (Eq, Show)

jw :: Options -> ReaderT Environment IO ()
jw Options {..} = case optCommand of
  Entry EntryOptions {..} -> case entryOptDay of
    Nothing -> do
      day <- asks envDay
      lift $ entry day
    Just day ->
      lift $ entry day

  View ViewOptions {..} -> case viewOptDay of
    Nothing -> do
      day <- asks envDay
      lift $ view day
    Just day ->
      lift $ view day

options :: Parser Options
options = Options <$> (helper <*> commandParser)
  where
    commandParser = hsubparser $
         command "entry" (info entryParser entryDesc)
      <> command "view" (info viewParser viewDesc)

    entryDesc    = progDesc "Start or continue an entry."
    entryParser  =
          Entry . EntryOptions
      <$> optional (argument auto
            (help "foo" <> metavar "DATE"))

    viewDesc   = progDesc "View an entry."
    viewParser =
          View . ViewOptions
      <$> optional (argument auto
            (help "foo" <> metavar "DATE"))

entry :: Day -> IO ()
entry day = do
  writeDir <- getWriteDir
  let fname = show day <.> "md"
      file  = writeDir </> fname
  wcvim <- getDataFileName "etc/wc.vim"
  csvim <- getDataFileName "etc/commands.vim"
  callProcess "vim"
    [ "-S", wcvim
    , "-s", csvim
    , file, "+"
    ]

view :: Day -> IO ()
view day = do
  writeDir <- getWriteDir
  let fname = show day <.> "md"
      file  = writeDir </> fname
  exists <- doesFileExist file
  if   exists
  then callProcess "less" [file]
  else hPutStrLn stderr $ "jw: no entry for " ++ show day

getWriteDir :: IO FilePath
getWriteDir = do
  home <- getHomeDirectory
  let writeDir = home </> ".jw" </> "entries"
  createDirectoryIfMissing True writeDir
  return writeDir

getEnvironmentInfo :: IO Environment
getEnvironmentInfo = do
  zone <- getCurrentTimeZone
  time <- getCurrentTime
  let envDay = localDay $ utcToLocalTime zone time

  envEditor <- lookupEnv "EDITOR"
  return Environment {..}

main :: IO ()
main = do
  env  <- getEnvironmentInfo
  opts <- execParser $ info options fullDesc
  runReaderT (jw opts) env

