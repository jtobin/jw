{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Time
import Options.Applicative as OA
import Paths_jw
import System.Directory
import System.Environment
import System.FilePath
import System.Process

data Options = Options {
    optCommand :: Command
  } deriving (Eq, Show)

data Environment = Environment {
    envDay    :: Day
  , envEditor :: Maybe String
  } deriving (Eq, Show)

data Command =
    Entry (Maybe Day)
  deriving (Eq, Show)

data Editor =
    Vim
  deriving (Eq, Show)

jw :: Options -> ReaderT Environment IO ()
jw Options {..} = case optCommand of
  Entry mday -> case mday of
    Nothing -> do
      day <- asks envDay
      lift $ entry day
    Just day ->
      lift $ entry day

options :: Parser Options
options = Options <$> (helper <*> entryCommand)
  where
    entryDesc    = progDesc "Start or continue an entry."
    entryCommand = hsubparser $
      command "entry" (info entryParser entryDesc)
    entryParser  =
          Entry
      <$> optional (argument auto
            (help "foo" <> metavar "DATE"))

entry :: Day -> IO ()
entry day = do
  writeDir <- createWriteDir
  let fname = show day <.> "md"
      file  = writeDir </> fname
  wcvim <- getDataFileName "etc/wc.vim"
  csvim <- getDataFileName "etc/commands.vim"
  callProcess "vim"
    [ "-S", wcvim
    , "-s", csvim
    , file, "+"
    ]

createFileName :: IO FilePath
createFileName = do
  zone <- getCurrentTimeZone
  time <- getCurrentTime
  let day = localDay (utcToLocalTime zone time)
  return $ show day <.> "md"

createWriteDir :: IO FilePath
createWriteDir = do
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

