module Main where

import qualified Text.PandocJira.Batch as Batch

import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<**>))

import Text.CSV (CSV)
import Text.Pandoc

import qualified Data.Text.IO as Text.IO

import qualified Options.Applicative as OptParse

import qualified Text.CSV as CSV


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

printOutputPath :: OutputPath -> String
printOutputPath StdOut = "stdout"
printOutputPath (OutputPath outputPath) = outputPath

outputFilePath :: OutputPath -> FilePath
outputFilePath StdOut = "/dev/stdout"
outputFilePath (OutputPath outputPath) = outputPath


main :: IO ()
main = run =<< OptParse.execParser optParser
  where
    optParser = OptParse.info (opts <**> OptParse.helper) info

-- HELPERS

run :: Options -> IO ()
run options =
    runIOorExplode $ do
        let (InputPath inputPath) = inputFile options
            outputPath = outputFile options
        doc <- loadMarkdown inputPath
        let batch = Batch.parse doc
        csv <- Batch.toCsv batch
        liftIO . writeFile (outputFilePath outputPath) . printCsv $ csv

loadMarkdown :: FilePath -> PandocIO Pandoc
loadMarkdown filepath = do
    fileContents <- liftIO . Text.IO.readFile $ filepath
    let options = def
            { readerExtensions = pandocExtensions
            }
    readMarkdown options fileContents

printCsv :: CSV -> String
printCsv = addNewline . CSV.printCSV
  where
    addNewline s
      | last s == '\n' = s
      | otherwise      = s ++ "\n"

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
inputArg = fmap InputPath . OptParse.strArgument
    $  OptParse.action "file"
    <> OptParse.metavar "INPUT"
    <> OptParse.help "Path to input markdown file to convert"

outputOpt :: OptParse.Parser OutputPath
outputOpt = OptParse.option (OutputPath <$> OptParse.str)
    $  OptParse.short 'o'
    <> OptParse.long "output"
    <> OptParse.action "file"
    <> OptParse.metavar "OUTPUT"
    <> OptParse.help "Path to output csv file path"
    <> OptParse.value StdOut
    <> OptParse.showDefaultWith printOutputPath
