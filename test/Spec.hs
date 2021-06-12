{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

import Test.Hspec

import Data.Text (Text)

import Text.PandocJira.Issue (Issue (..))
import Text.PandocJira.Batch (Batch (..))
import Text.Pandoc (Pandoc (..))

import qualified Text.PandocJira.Batch as Batch
import qualified Text.PandocJira.Issue as Issue

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Text.Pandoc as Pandoc


main =
    hspec $ do
        describe "Issue.parseAll" $
            mapM_ testParseIssues tests
        describe "Batch.parse" $ do
            mapM_ testParseBatch tests


data Test = Test
    { testSummary :: String
    , inputFile :: FilePath
    , descriptionFiles :: [Maybe FilePath]
    , issueSummaries :: [Text]
    , issueMetadata :: [Issue.Metadata]
    , batchAttributes :: [Text]
    }

tests =
    [ Test
        { testSummary = "Can parse a single summary"
        , inputFile = "test/one-summary.md"
        , descriptionFiles = [Nothing]
        , issueSummaries = ["This is a summary"]
        , issueMetadata = [[]]
        , batchAttributes = []
        }
    , Test
        { testSummary = "Can parse two summaries"
        , inputFile = "test/two-summary.md"
        , descriptionFiles = replicate 2 Nothing
        , issueSummaries = ["This is one summary", "This is two summary"]
        , issueMetadata = replicate 2 []
        , batchAttributes = []
        }
    , Test
        { testSummary = "Can parse three summaries"
        , inputFile = "test/three-summary.md"
        , descriptionFiles = replicate 3 Nothing
        , issueSummaries =
            [ "This is one summary"
            , "This is two summary"
            , "This is three summary"
            ]
        , issueMetadata = replicate 3 []
        , batchAttributes = []
        }
    , Test
        { testSummary = "Can parse a single description"
        , inputFile = "test/one-description.md"
        , descriptionFiles = [Just "test/one-description-d.md"]
        , issueSummaries = ["This is a summary"]
        , issueMetadata = [[]]
        , batchAttributes = []
        }
    , Test
        { testSummary = "Can parse two descriptions"
        , inputFile = "test/two-description.md"
        , descriptionFiles =
            [ Just "test/two-description-d1.md"
            , Just "test/two-description-d2.md"
            ]
        , issueSummaries = ["Summary one", "Summary two"]
        , issueMetadata = replicate 2 []
        , batchAttributes = []
        }
    , Test
        { testSummary = "Can parse one description with metadata"
        , inputFile = "test/one-metadata.md"
        , descriptionFiles = [Just "test/one-metadata-d.md"]
        , issueSummaries = ["Summary one"]
        , issueMetadata =
            [[("Priority", "Highest"), ("Epic Link", "Jira-123")]]
        , batchAttributes = ["Priority", "Epic Link"]
        }
    , Test
        { testSummary = "Can parse metadata within the description"
        , inputFile = "test/inner-metadata.md"
        , descriptionFiles = [Just "test/inner-metadata-d.md"]
        , issueSummaries = ["Summary one"]
        , issueMetadata =
            [[("Author", "Calvin"), ("Priority", "5")]]
        , batchAttributes = ["Author", "Priority"]
        }
    , Test
        { testSummary = "Can parse two metadata"
        , inputFile = "test/two-metadata.md"
        , descriptionFiles =
            [ Just "test/two-metadata-d1.md"
            , Just "test/two-metadata-d2.md"
            ]
        , issueSummaries = ["Summary one", "Summary two"]
        , issueMetadata =
            [ [ ("Issue Type", "Task")
              , ("Priority", "Low")
              ]
            , [ ("Issue Type", "Bug")
              , ("Priority", "High")
              , ("Attachment", "file://image-name1.png")
              , ("Attachment", "file://image-name2.png")
              ]
            ]
        , batchAttributes = ["Issue Type", "Priority", "Attachment", "Attachment"]
        }
    , Test
        { testSummary = "Can parse multiple metadata from the same term"
        , inputFile = "test/multi-meta.md"
        , descriptionFiles = [Nothing]
        , issueSummaries = ["Attach the files used in Jira documentation"]
        , issueMetadata =
            [ [ ("Attachment", "https://jira-server:8080/secure/attachment/image-name.png")
              , ("Attachment", "01/01/2012 13:10;Admin;image.png;file://image-name.png")
            ]]
        , batchAttributes = ["Attachment", "Attachment"]
        }
    ]


testParseIssues :: Test -> SpecWith (Arg Expectation)
testParseIssues (Test {testSummary, inputFile, descriptionFiles, issueSummaries, issueMetadata}) =
    it testSummary $ do
        let maybeLoadMarkdown = maybe (return Issue.emptyDesc) loadMarkdown
        issueDescriptions <- mapM maybeLoadMarkdown descriptionFiles
        let expected = zipWith3 Issue issueSummaries issueDescriptions issueMetadata
        inputFile `shouldParseToIssues` expected

testParseBatch :: Test -> SpecWith (Arg Expectation)
testParseBatch (Test {testSummary, inputFile, descriptionFiles, issueSummaries, issueMetadata, batchAttributes}) =
    it testSummary $ do
        let maybeLoadMarkdown = maybe (return Issue.emptyDesc) loadMarkdown
        issueDescriptions <- mapM maybeLoadMarkdown descriptionFiles
        let issues = zipWith3 Issue issueSummaries issueDescriptions issueMetadata
            expected = Batch.fromIssuesAttributes issues batchAttributes
        inputFile `shouldParseToBatch` expected


shouldParseToIssues :: FilePath -> [Issue] -> Expectation
shouldParseToIssues filepath expected = do
     pandoc <- loadMarkdown filepath
     let actual = Issue.parseAll . pandocBlocks $ pandoc
     actual `shouldBe` expected

shouldParseToBatch :: FilePath -> Batch -> Expectation
shouldParseToBatch filepath expected = do
    pandoc <- loadMarkdown filepath
    let actual = Batch.parse pandoc
    actual `shouldBe` expected


loadMarkdown :: FilePath -> IO Pandoc
loadMarkdown filepath = do
    fileContents <- Text.IO.readFile filepath
    let options = Pandoc.def
            { Pandoc.readerExtensions = Pandoc.pandocExtensions
            }
    Pandoc.runIOorExplode (Pandoc.readMarkdown options fileContents)

pandocBlocks :: Pandoc -> [Pandoc.Block]
pandocBlocks (Pandoc _meta bs) =
    bs
