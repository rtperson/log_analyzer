{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module GetLogs where

import Data.Maybe
import System.IO
import Network.HTTP
import Network.URI
import qualified Data.ByteString.Lazy as BS
import Codec.Archive.Zip
import Text.Regex.Posix
import Data.Either
import Data.List
import Time
import Locale
import Control.Exception
import Text.XML.HXT.Core


zipFile = "test.zip"

filePat = "TESTZIP[0-9]-FILE-${timePat}_.{8}.zip"

cnn = "http://www.cnn.com" :: String
reddit = "http://www.reddit.com" :: String

localZip :: String
localZip = "loc.zip"

simpleRequest url = Request {
                    rqURI     = uri
                  , rqMethod  = GET
                  , rqHeaders = []
                  , rqBody    = ""
                  }
  where
    uri = fromJust $ parseURI url
    


downloadPage :: Request_String ->  IO (Either String String)
downloadPage rq = do
    resp <- simpleHTTP rq
    case resp of 
        Left x -> return $ Left ("Error connecting: " ++ show x)
        Right r -> 
            case rspCode r of
                (2,_,_) -> return $ Right (rspBody r)
                (3,_,_) -> -- HTTP Redirect
                    case findHeader HdrLocation r of
                        Nothing -> return $ Left (show r)
                        Just url -> do
                                  let rq2 = simpleRequest url
                                  downloadPage rq2 
                (4,_,_) -> return $ Left ("Could not find")
                                  

            
-- for download of a binary file to be stored locally.            
downloadFile :: Request_String -> IO (Maybe String)
downloadFile rq = do
    resp <- downloadPage rq
    case resp of 
        Left x -> do putStrLn x
                     return Nothing
        Right doc -> do
            file <- openBinaryFile localZip WriteMode
            hPutStr file doc
            hClose file
            return $ Just localZip
       
-- extract a zip file       
extractFile :: FilePath -> IO ()
extractFile path = do
    arFile <- BS.readFile path
    let archive = toArchive arFile
    extractFilesFromArchive [OptVerbose] archive
    
-- get a list of files from an HTML page
getHtml :: String -> IO String
getHtml url = do
    html <- downloadPage $ simpleRequest url
    file <- filenamePat
    --let matchCit t = (t =~ file :: Bool) == True
    case html of
        Left x -> do putStrLn x
                     return []
        Right doc -> do
                     --let list = words doc
                     return $ doc
                     
         
       
saveHtml :: IO ()
saveHtml = do
    outh <- openFile "html.txt" WriteMode
    html <- getHtml eds01
    hPutStr outh html
    hFlush outh
    hClose outh

   
getCurrentCal :: IO CalendarTime   
getCurrentCal = toCalendarTime =<< getClockTime

filenamePat :: IO String
filenamePat = do
    now <- getCurrentCal
    let timePat = formatCalendarTime defaultTimeLocale "%m-%d-%Y" now
    let filePat = "TESTZIP[0-9]-FILE-" ++ timePat ++ "_.{8}.zip"
    return filePat
 
                    
simplerHtmlSearch = do
    html <- downloadPage $ simpleRequest reddit
    case html of 
        Left err -> putStrLn err
        Right htm -> do
            let doc = readString [withParseHTML yes, withWarnings no] htm
            links <- runX $ doc //> hasName "a" >>> getAttrValue "href"
            mapM_ putStrLn links      
    
main = do
    e <- downloadFile $ simpleRequest zipFile
    case e of 
        Nothing -> return ()
        Just _  -> extractFile localZip
    return ()
