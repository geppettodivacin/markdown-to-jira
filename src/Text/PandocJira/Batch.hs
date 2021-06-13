{-# LANGUAGE OverloadedStrings #-}

module Text.PandocJira.Batch (Batch (..), fromIssuesAttributes, parse, toCsv) where

import Text.PandocJira.Issue (Issue (..))

import Control.Monad (mapM)
import Data.List (foldl', lookup)
import Data.Maybe (fromMaybe)
import Data.MultiSet (MultiSet)
import Data.Text (Text)

import Text.Pandoc (Pandoc (..), PandocMonad)
import Text.CSV (CSV)

import qualified Data.MultiSet as MultiSet
import qualified Data.Text as Text
import qualified Text.PandocJira.Issue as Issue
import qualified Text.Pandoc as Pandoc

import Debug.Trace


data Batch =
    Batch
    { issues :: [Issue]
    , attributes :: MultiSet Text
    }
    deriving (Show, Eq)


fromIssuesAttributes :: [Issue] -> [Text] -> Batch
fromIssuesAttributes issues attributes = Batch issues (MultiSet.fromList attributes)


parse :: Pandoc -> Batch
parse pandoc =
    Batch issues attributes
  where
    issues = Issue.parseAll . pandocBlocks $ pandoc
    attributes = foldr MultiSet.maxUnion MultiSet.empty . map (MultiSet.fromList . map fst . Issue.metadata) $ issues


toCsv :: PandocMonad m => Batch -> m CSV
toCsv (Batch issues attributes) = do
    descriptions <- mapM descriptionToJira issues
    return $ csvData (issues, descriptions)
  where
    csvData :: ([Issue], [Text]) -> CSV
    csvData (issues, descriptions) = map (map Text.unpack) $
        [header] ++ map toRow (zip issues descriptions)

    attributeList :: [Text]
    attributeList = MultiSet.toList attributes

    header :: [Text]
    header = [ "Summary", "Description" ] ++ attributeList

    descriptionToJira :: PandocMonad m => Issue -> m Text
    descriptionToJira issue = Pandoc.writeJira Pandoc.def . Issue.description $ issue

    toRow :: (Issue, Text) -> [Text]
    toRow (Issue {summary = summary, metadata = metadata}, description) =
        [ summary, description ] ++ metadataColumns metadata attributeList

--- UTILITY

pandocBlocks :: Pandoc -> [Pandoc.Block]
pandocBlocks (Pandoc _meta bs) =
    bs


metadataColumns :: Issue.Metadata -> [Text] -> [Text]
metadataColumns metadata attributes = snd $ go metadata attributes
  where
    go :: Issue.Metadata -> [Text] -> (Issue.Metadata, [Text])
    go metadata attributes = foldl' f (metadata, []) attributes

    f :: (Issue.Metadata, [Text]) -> Text -> (Issue.Metadata, [Text])
    f (metadata, values) attribute =
        let (value, metadata') = popLookup attribute metadata
        in  (metadata', values ++ [(fromMaybe "" value)])


popLookup :: Eq a => a -> [(a, b)] -> (Maybe b, [(a, b)])
popLookup x [] = (Nothing, [])
popLookup x ((x1, y1) : entries)
  | x == x1   = (Just y1, entries)
  | otherwise = let (value, entries') = popLookup x entries
                in  (value, (x1, y1) : entries')
