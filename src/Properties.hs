-- |The purpose of this module is to read an external properties file into
--  a Haskell Data.Map structure, such that the file can be structured as so:
--
--    site=www.hostname.com
--    port=8080
--    protocol=http
--
--  using the left hand side term as the key and the right hand side as the 
--  value of the map.
--
--  Or, more formally: 
--     key   ::= var
--     value ::= var | num
--     sepr  ::= =
--     var   ::= letter { letter | digit }*
--     num   ::= digit { digit }*

module Properties where

import System.IO
import System.FilePath
import Data.Map
import qualified Data.ByteString.Char8 as B
import Control.Exception (bracket)
import System.Environment
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

-- begin the very clumsy attempts to play around with Parsec. All my parser combinators 
-- look the same because they are the result of speed-reading "Write Yourself a Scheme in 48 Hours"
-- A few todos: 1) use Overloaded Strings so we can read in the file as either ByteString or [Char]
--              2) Extend logic so that it can read in a multi-line file 
--              3) And assign the keys and values to a dictionary (a la Data.Map)
--              4) Create unit tests for this functionality

logFile = "testlog.properties"

data Props = Key String
           | Separ
           | ValNum Int
           | ValAlph String
           deriving Show
           

           
parseKey :: Parser Props
parseKey = do first <- letter 
              rest <- many (letter <|> digit)
              let propKey = first:rest
              return $ Key propKey
              
parseSepar :: Parser Props
parseSepar = do char '='
                return Separ
                
parseVal :: Parser Props
parseVal = parseAlphVal <|> parseNumVal

parseNumVal :: Parser Props
parseNumVal = do first <- digit
                 rest <- many digit
                 let ret = read (first:rest)
                 return $ ValNum ret

parseAlphVal :: Parser Props
parseAlphVal = do first <- letter
                  rest <- many anyChar
                  return $ ValAlph (first:rest)
           
parseProps :: Parser [Props]
parseProps = do first <- parseKey
                sep <- parseSepar
                val <- parseVal
                return $ (first:sep:val:[])



main :: IO ()
main = do
    key <- parseFromFile parseProps logFile
    case key of
        Left err -> putStrLn $ "received err: " ++ (show err)
        Right v  -> putStrLn $ "Here's the key: " ++ (show v)
    return ()



