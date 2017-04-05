{-# LANGUAGE TemplateHaskell #-}

module Main where

import Codec.Archive.Zip
import Control.Monad ((<=<))
import Control.Monad.IO.Class
import Data.ByteString.Char8 (unpack, pack)
import Data.List
import Path -- (parseRelFile)
import Path.IO (resolveFile')
import Text.XML.HXT.Core hiding (addEntry)
import System.Environment
import System.Exit


main :: IO ()
main = getArgs >>= mapM_ (process <=< resolveFile')

process file = withArchive file $ do
    relsfile <- mkEntrySelector $(mkRelFile "word/_rels/document.xml.rels")
    xml <- unpack <$> getEntry relsfile
    [newxml] <- liftIO . runX $ processXML xml
    addEntry Deflate (pack newxml) relsfile

processXML xml = readString [] xml
             >>> fixHyperlinks
             >>> writeDocumentToString []

fixHyperlinks :: ArrowXml a => a XmlTree XmlTree
fixHyperlinks = processTopDown
  $ processAttrl (changeAttrValue replaceSpace `when` hasName "Target")
    `when` (isElem >>> hasName "Relationship")

replaceSpace :: String -> String
replaceSpace [] = []
replaceSpace (' ':cs) = "%20" ++ replaceSpace cs
replaceSpace (c  :cs) = c      : replaceSpace cs
