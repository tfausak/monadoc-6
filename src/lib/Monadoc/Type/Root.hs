{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Root where

import qualified Data.Text as Text
import qualified Monadoc.Class.ToXml as ToXml
import qualified Monadoc.Type.Meta as Meta
import qualified Monadoc.Type.Route as Route
import qualified Monadoc.Utility.Xml as Xml
import qualified Text.XML as Xml
import qualified Witch

data Root page = Root
    { meta :: Meta.Meta
    , page :: page
    } deriving (Eq, Show)

toDocument :: ToXml.ToXml page => Root page -> Xml.Document
toDocument root =
    let
        href = Xml.escape $ Meta.baseUrl (meta root) <> Route.toString Route.Template
        instruction = Xml.Instruction
            (Witch.into @Text.Text "xml-stylesheet")
            (Witch.into @Text.Text $ "type=\"text/xsl\" charset=\"UTF-8\" href=\"" <> href <> "\"")
    in Xml.Document
        (Xml.Prologue [Xml.MiscInstruction instruction] Nothing [])
        (toElement root)
        []

toElement :: ToXml.ToXml page => Root page -> Xml.Element
toElement root = Xml.element "root" []
    [ ToXml.toXml $ meta root
    , Xml.node "page" [] [ToXml.toXml $ page root]
    ]
