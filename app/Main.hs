module Main where

import Codec.Archive.Zip
import Control.Monad.IO.Class
import Data.ByteString.Char8 (unpack, pack)
import Data.List
import Path -- (parseRelFile)
import Path.IO (resolveFile')
import Text.XML.HXT.Core hiding (addEntry)
import System.Environment
import System.Exit



main :: IO ()
main = do
  file <- resolveFile' . head =<< getArgs

  worddir <- parseRelDir "word"
  relsdir <- parseRelDir "_rels"
  xmlfile <- parseRelFile "document.xml.rels"

  sel <- mkEntrySelector $ worddir </> relsdir </> xmlfile

  withArchive file $ do

    xml <- unpack <$> getEntry sel

    [newxml] <- liftIO . runX $ application xml

    liftIO $ putStrLn xml
    liftIO $ putStrLn "====================================="
    liftIO $ putStrLn newxml

    addEntry Deflate (pack newxml) sel

    return ()


  --exitSuccess
  -- [rc] <- runX (application file1 file2)
  -- if rc >= c_err
  --   then exitWith (ExitFailure (-1))
  --   else exitSuccess


application xml = readString [] xml
              >>> fixHyperlinks
              >>> writeDocumentToString []


fixHyperlinks :: ArrowXml a => a XmlTree XmlTree
fixHyperlinks = processTopDown editHyperlink
  where
    editHyperlink
      = processAttrl (changeAttrValue replaceSpace `when` hasName "Target")
       `when`
       (isElem >>> hasName "Relationship")


replaceSpace :: String -> String
replaceSpace [] = []
replaceSpace (' ':cs) = "%20" ++ replaceSpace cs
replaceSpace (c  :cs) = c      : replaceSpace cs
