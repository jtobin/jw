{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Time
import Options.Applicative as OA
import Paths_jw
import System.Directory
import System.FilePath
import System.Process

data Options = Options {
    optCommand :: Command
  } deriving (Eq, Show)

data Command =
    New
  deriving (Eq, Show)

jw :: Options -> IO ()
jw Options {..} = case optCommand of
  New -> newEntry

opts :: Parser Options
opts = Options <$>
    subparser newCommand
  where
    newParser  = pure New
    newDesc    = progDesc "Write a new entry"
    newCommand = command "new" (info newParser newDesc)

newEntry :: IO ()
newEntry = do
  file  <- liftA2 (</>) createWriteDir createFileName
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
  let writeDir = home </> ".jw"
  createDirectoryIfMissing True writeDir
  return writeDir

main :: IO ()
main = do
  options <- execParser (info opts fullDesc)
  jw options

