{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Class.ToXml where

import Monadoc.Prelude

import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Void as Void
import qualified Documentation.Haddock.Types as Haddock
import qualified Monadoc.Utility.Xml as Xml
import qualified Text.XML as Xml

class ToXml a where
    toXml :: a -> Xml.Node

instance ToXml String where
    toXml = toXml . into @Text.Text

instance ToXml a => ToXml (Maybe a) where
    toXml = maybe (toXml $ into @Text.Text "") toXml

instance ToXml Int where
    toXml = toXml . show

instance ToXml Word where
    toXml = toXml . show

instance ToXml Text.Text where
    toXml = Xml.NodeContent

instance ToXml Time.UTCTime where
    toXml = toXml . Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ"

instance ToXml Bool where
    toXml p = toXml $ if p then "true" else ""

instance ToXml Int.Int64 where
    toXml = toXml . show

instance (ToXml mod, ToXml id) => ToXml (Haddock.DocH mod id) where
    toXml x = case x of
        Haddock.DocAName y -> Xml.node "docAName" [] [toXml y]
        Haddock.DocAppend y z -> Xml.node "docAppend" [] [toXml y, toXml z]
        Haddock.DocBold y -> Xml.node "docBold" [] [toXml y]
        Haddock.DocCodeBlock y -> Xml.node "docCodeBlock" [] [toXml y]
        Haddock.DocDefList y -> Xml.node "docDefList" [] $ foldMap (\ (k, v) -> [Xml.node "term" [] [toXml k], Xml.node "description" [] [toXml v]]) y
        Haddock.DocEmphasis y -> Xml.node "docEmphasis" [] [toXml y]
        Haddock.DocEmpty -> Xml.node "docEmpty" [] []
        Haddock.DocExamples y -> Xml.node "docExamples" [] $ fmap toXml y
        Haddock.DocHeader y -> Xml.node "docHeader" [] [toXml y]
        Haddock.DocHyperlink y -> Xml.node "docHyperlink" [] [toXml y]
        Haddock.DocIdentifier y -> Xml.node "docIdentifier" [] [toXml y]
        Haddock.DocIdentifierUnchecked y -> Xml.node "docIdentifierUnchecked" [] [toXml y]
        Haddock.DocMathDisplay y -> Xml.node "docMathDisplay" [] [toXml y]
        Haddock.DocMathInline y -> Xml.node "docMathInline" [] [toXml y]
        Haddock.DocModule y -> Xml.node "docModule" [] [toXml y]
        Haddock.DocMonospaced y -> Xml.node "docMonospaced" [] [toXml y]
        Haddock.DocOrderedList y -> Xml.node "docOrderedList" [] $ fmap toXml y
        Haddock.DocParagraph y -> Xml.node "docParagraph" [] [toXml y]
        Haddock.DocPic y -> Xml.node "docPic" [] [toXml y]
        Haddock.DocProperty y -> Xml.node "docProperty" [] [toXml y]
        Haddock.DocString y -> Xml.node "docString" [] [toXml y]
        Haddock.DocTable y -> Xml.node "docTable" [] [toXml y]
        Haddock.DocUnorderedList y -> Xml.node "docUnorderedList" [] $ fmap toXml y
        Haddock.DocWarning y -> Xml.node "docWarning" [] [toXml y]

instance ToXml Haddock.Example where
    toXml x = Xml.node "example" []
        [ Xml.node "expression" [] [toXml $ Haddock.exampleExpression x]
        , Xml.node "results" [] . fmap (\ y -> Xml.node "result" [] [toXml y]) $ Haddock.exampleResult x
        ]

instance ToXml id => ToXml (Haddock.Header id) where
    toXml x = Xml.node "header" []
        [ Xml.node "level" [] [toXml $ Haddock.headerLevel x]
        , Xml.node "title" [] [toXml $ Haddock.headerTitle x]
        ]

instance ToXml id => ToXml (Haddock.Hyperlink id) where
    toXml x = Xml.node "hyperlink" []
        [ Xml.node "label" [] [toXml $ Haddock.hyperlinkLabel x]
        , Xml.node "url" [] [toXml $ Haddock.hyperlinkUrl x]
        ]

instance ToXml id => ToXml (Haddock.ModLink id) where
    toXml x = Xml.node "modLink" []
        [ Xml.node "label" [] [toXml $ Haddock.modLinkLabel x]
        , Xml.node "name" [] [toXml $ Haddock.modLinkName x]
        ]

instance ToXml Haddock.Picture where
    toXml x = Xml.node "picture" []
        [ Xml.node "title" [] [toXml $ Haddock.pictureTitle x]
        , Xml.node "uri" [] [toXml $ Haddock.pictureUri x]
        ]

instance ToXml id => ToXml (Haddock.Table id) where
    toXml x = Xml.node "table" []
        [ Xml.node "headerRows" [] . fmap toXml $ Haddock.tableHeaderRows x
        , Xml.node "bodyRows" [] . fmap toXml $ Haddock.tableBodyRows x
        ]

instance ToXml id => ToXml (Haddock.TableRow id) where
    toXml = Xml.node "tableRow" [] . fmap toXml . Haddock.tableRowCells

instance ToXml id => ToXml (Haddock.TableCell id) where
    toXml x = Xml.node "tableCell" []
        [ Xml.node "colspan" [] [toXml $ Haddock.tableCellColspan x]
        , Xml.node "contents" [] [toXml $ Haddock.tableCellContents x]
        , Xml.node "rowspan" [] [toXml $ Haddock.tableCellRowspan x]
        ]

instance ToXml Void.Void where
    toXml = Void.absurd

instance ToXml Xml.Node where
    toXml = id
