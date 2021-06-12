module Text.PandocJira.Issue (Issue (..), Metadata, parse, parseAll, emptyDesc) where

import Data.Monoid (mempty)
import Data.Text (Text)
import Text.Pandoc (Pandoc (..))

import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Shared as Pandoc
import qualified Data.Text as Text


data Issue =
    Issue
    { summary :: Text
    , description :: Pandoc
    , metadata :: Metadata
    }
    deriving (Show, Eq)

type Metadata =
    [(Text, Text)]

parse :: [Pandoc.Block] -> (Maybe Issue, [Pandoc.Block])
parse = parseSummary

parseSummary :: [Pandoc.Block] -> (Maybe Issue, [Pandoc.Block])
parseSummary (block : blocks) =
    case block of
        Pandoc.Header 1 _ inline ->
            case parseDescription (Pandoc.stringify inline) blocks of
                (issue, blocks) ->
                    (Just issue, blocks)
        _ -> (Nothing, blocks)

parseDescription :: Text -> [Pandoc.Block] -> (Issue, [Pandoc.Block])
parseDescription summary blocks =
    (issue, remaining)
  where
    (issue, remaining) = go blocks

    init = Issue summary emptyDesc []

    go :: [Pandoc.Block] -> (Issue, [Pandoc.Block])
    go [] =
        (init, [])
    go (block : blocks) =
        case block of
            Pandoc.Header 1 _ _ ->
                -- We've found the end of this issue, stop recursion
                (init, block : blocks)
            Pandoc.Header n attr inline ->
                -- Shift all headers up a level
                let (acc, blocks') = go blocks in
                (prependToDescription (Pandoc.Header (n - 1) attr inline) acc, blocks')
            Pandoc.DefinitionList definitions ->
                -- Append metadata to the end
                -- NOTE: build metadata in reverse, so later values are preferred
                let (acc, blocks') = go blocks
                    entries = concatMap toEntries definitions
                    acc' = prependToMetadata entries acc in
                (acc', blocks')
            block ->
                -- Add every other block type to the description
                let (acc, blocks') = go blocks in
                (prependToDescription block acc, blocks')

parseAll :: [Pandoc.Block] -> [Issue]
parseAll [] =
    []
parseAll blocks =
    case parse blocks of
        (Nothing, blocks') -> parseAll blocks'
        (Just issue, blocks') -> issue : parseAll blocks'

emptyDesc :: Pandoc
emptyDesc =
    mempty


-- Private functions
prependToPandoc :: Pandoc.Block -> Pandoc -> Pandoc
prependToPandoc block (Pandoc meta blocks) =
    Pandoc meta (block : blocks)

prependToDescription :: Pandoc.Block -> Issue -> Issue
prependToDescription block issue =
    issue { description = prependToPandoc block (description issue) }

prependToMetadata :: [(Text, Text)] -> Issue -> Issue
prependToMetadata entries issue =
    issue { metadata = entries ++ metadata issue }

toEntries :: ([Pandoc.Inline], [[Pandoc.Block]]) -> [(Text, Text)]
toEntries (term, definitions) =
    -- TODO: Add proper exception throw here, to be caught by IO
    map (withKey . toValue) definitions
  where
    key =
        Pandoc.stringify term

    toValue :: [Pandoc.Block] -> Text
    toValue definition =
        Pandoc.stringify definition

    withKey :: Text -> (Text, Text)
    withKey value =
        (key, value)
