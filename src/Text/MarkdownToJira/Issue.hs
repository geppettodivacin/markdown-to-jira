module Text.MarkdownToJira.Issue (Issue (..), Metadata, parse, parseAll, emptyDesc) where

import Control.Monad.State (State)
import Data.Monoid (mempty)
import Data.Text (Text)
import Text.Pandoc (Pandoc (..))

import qualified Control.Monad.State as State
import qualified Control.Monad.Loops as Loops
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Shared as Pandoc


data Issue =
    Issue
    { summary :: Text
    , description :: Pandoc
    , metadata :: Metadata
    }
    deriving (Show, Eq)

type Metadata =
    [(Text, Text)]

parse :: State [Pandoc.Block] (Maybe Issue)
parse = do
    maybeSummary <- parseSummary
    case maybeSummary of
        Nothing ->
            return Nothing
        Just summary -> do
            (description, attributes) <- parseBody
            return . Just $ Issue summary description attributes

parseSummary :: State [Pandoc.Block] (Maybe Text)
parseSummary = do
    blocks <- State.get
    let (summary, blocks') =
            case blocks of
                (Pandoc.Header 1 _ inline : blocks') ->
                    (Just $ Pandoc.stringify inline, blocks')
                (_ : blocks') ->
                    (Nothing, blocks')
                _ ->
                    (Nothing, [])
    State.put blocks'
    return summary

parseBody :: State [Pandoc.Block] (Pandoc, Metadata)
parseBody = do
    blocks <- State.get
    let (result, remaining) = go blocks
    State.put remaining
    return result
  where
    init = (emptyDesc, [])

    go :: [Pandoc.Block] -> ((Pandoc, Metadata), [Pandoc.Block])
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
    Maybe.catMaybes maybeIssues
  where
    maybeIssues = State.evalState (parse `Loops.untilM` State.gets null) blocks

emptyDesc :: Pandoc
emptyDesc =
    mempty


-- Private functions
type Body = (Pandoc, Metadata)

prependToPandoc :: Pandoc.Block -> Pandoc -> Pandoc
prependToPandoc block (Pandoc meta blocks) =
    Pandoc meta (block : blocks)

prependToDescription :: Pandoc.Block -> Body -> Body
prependToDescription block (description, metadata) =
    (prependToPandoc block description, metadata)

prependToMetadata :: [(Text, Text)] -> Body -> Body
prependToMetadata entries (description, metadata) =
    (description, entries ++ metadata)

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
