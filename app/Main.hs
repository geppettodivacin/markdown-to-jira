module Main where

import qualified Text.PandocJira.Batch as Batch

import Control.Monad.IO.Class (liftIO)

import Text.CSV
import Text.Pandoc

import qualified Data.Text.IO as Text.IO

import qualified Options.Applicative as OptParse


data Options = Options
    { inputFile :: InputPath
    , outputFile :: OutputPath
    }

newtype InputPath = InputPath FilePath
    deriving Show

data OutputPath
    = StdOut
    | OutputPath FilePath
    deriving Show


main :: IO ()
main = run =<< OptParse.execParser optParser
  where
    optParser = OptParse.info opts info

-- HELPERS

run :: Options -> IO ()
run options =
    runIOorExplode $ do
        doc <- loadMarkdown "in.md"
        let batch = Batch.parse doc
        csv <- Batch.toCsv batch
        liftIO . putStrLn . printCSV $ csv

loadMarkdown :: FilePath -> PandocIO Pandoc
loadMarkdown filepath = do
    fileContents <- liftIO . Text.IO.readFile $ filepath
    let options = def
            { readerExtensions = pandocExtensions
            }
    readMarkdown options fileContents

-- OPTION PARSERS

info :: OptParse.InfoMod a
info =
    OptParse.fullDesc <> progDesc
  where
    progDesc = OptParse.progDesc "Converts input markdown to a Jira-consumable CSV file"

opts :: OptParse.Parser Options
opts =
    Options <$> inputArg <*> outputOpt

inputArg :: OptParse.Parser InputPath
inputArg = _

outputOpt :: OptParse.Parser OutputPath
outputOpt = _
