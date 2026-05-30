{-  Parsec stanje

    Funkcije
        - getState :: Monad m => ParsecT s u m u
        - putState :: Monad m => u -> ParsecT s u m ()
        - modifyState :: Monad m => (u -> u) -> ParsecT s u m ()
    
    "getState" vraca trenutnu vrednost stanja
    
    "putState" uzima vrednost i postavlja trenutnu vrednost stanja na datu vrednost
    
    "modifyState" uzima funkciju kojom modifikuje vrednost stanja
    
    Stanje moze biti bilo sta
    
    Monadic kontekst brine da stanje bude ocuvano
    
    Moze se koristiti State monad da bi se dobile naprednije mogucnosti

-}

-- primer Stanje

import Text.Parsec
import System.Environment
import System.IO

newtype Record = Record (String, String) deriving (Show)

fonS :: Parsec String Int Record
fonS = do ime <- many1 letter
          spaces
          br <- many1 digit
          cnt <- getState
          let cnt' = cnt + 1
          putState cnt'
          return (Record (ime, br))

separatorS :: Parsec String Int ()
separatorS = spaces >> char ',' >> spaces

imenikS :: Parsec String Int (Int, [Record])
imenikS = do res <- many $ do zapis <- fonS
                              eof <|> separatorS
                              return zapis
             cnt <- getState
             return (cnt, res)

-- runParser imenikS 0 "" "pera 1234, mica  2234 ,neko98798   ,  zeljko 234,"

{-

    Parsirati RSS feed sa http://www.b92.net/info/rss/novo.xml u JSON format
    
    Zadaju se putanje do ulaznog i izlaznog file-a

-}

-- primer XML

data RSS = RSS [Attribute] [Channel]  -- deriving (Show)

data Attribute = Attribute AttrName AttrValue NumTabs  -- deriving (Show)

data Channel = Channel [Tag] NumTabs -- deriving (Show)

data Tag = Element TagName [Attribute] [Tag] NumTabs
           | SCTag TagName [Attribute] NumTabs
           | Body String NumTabs  -- deriving (Show)

type AttrName = String
type AttrValue = String
type TagName = String
type NumTabs = Int

insertTabs:: Int -> String
insertTabs 0 = ""
insertTabs x = "\t" ++ insertTabs (x - 1)

instance Show RSS where
    show (RSS attrs chnls) = "{\n" ++ showAttr attrs ++ showChnl chnls ++ "}"
        where showAttr [] = ""
              showAttr xs = "\t\"atributes\": {\n" ++ attrHlpr xs ++ "\t},\n"
                where attrHlpr [] = ""
                      attrHlpr (x:xs) = show x ++ ",\n" ++ attrHlpr xs
              showChnl [] = ""
              showChnl xs = "\t\"channels\": [\n" ++ chnlHlpr xs ++ "\t]\n"
                where chnlHlpr [] = ""
                      chnlHlpr (x:xs) = "{\n" ++ show x ++ "\n},\n" ++ chnlHlpr xs

instance Show Channel where
    show (Channel tags tabs) = helper tags
        where helper [] = ""
              helper (x:xs) = show x ++ ",\n" ++ helper xs

instance Show Attribute where
    show (Attribute name value tabs) = insertTabs tabs ++ "\"" ++ name ++ "\": \"" ++ value ++ "\""

instance Show Tag where
    show (Body str tabs) = insertTabs tabs ++ "\"body\": \"" ++ str ++ "\""
    show (SCTag name attrs tabs) = insertTabs tabs ++ "\"" ++ name ++ "\": {\n" ++ showAttr attrs ++ insertTabs tabs ++ "}"
        where showAttr [] = ""
              showAttr xs = insertTabs tabs ++ "\t\"atributes\": {\n" ++ attrHlpr xs ++ insertTabs tabs ++ "\t}\n"
                where attrHlpr [] = ""
                      attrHlpr (x:xs) = show x ++ ",\n" ++ attrHlpr xs
    show (Element name attrs tags tabs) = insertTabs tabs ++ "\"" ++ name ++ "\": {\n" ++ showAttr attrs ++ showTags tags ++ insertTabs tabs ++ "}"
        where showAttr [] = ""
              showAttr xs = insertTabs tabs ++ "\t\"atributes\": {\n" ++ attrHlpr xs ++ insertTabs tabs ++ "\t},\n"
                where attrHlpr [] = ""
                      attrHlpr (x:xs) = show x ++ ",\n" ++ attrHlpr xs
              showTags [] = ""
              showTags (x:xs) = show x ++ ",\n" ++ showTags xs

document :: Parsec String Int RSS  -- ()
document = do spaces
              try xmlDecl
              spaces
              res <- rss
              spaces
              eof
              return res

xmlDecl :: Parsec String Int String  -- ()
xmlDecl = string "<?xml" >> many (noneOf "?>") >> string "?>"

rss :: Parsec String Int RSS  -- ()
rss = do char '<'
         spaces
         string "rss"
         spaces
         modifyState (+1)
         attrs <- many attribute
         modifyState (subtract 1)
         spaces
         char '>'
         spaces
         modifyState (+1)
         chnls <- many channel
         modifyState (subtract 1)
         spaces
         string "</"
         spaces
         string "rss"
         spaces
         char '>'
         return (RSS attrs chnls)

attribute :: Parsec String Int Attribute  -- ()
attribute = do name <- many (noneOf "/= >")
               spaces
               try (char '=')
               value <- between (char '"') (char '"') (many (noneOf ['"']))
               spaces
               Attribute name value <$> getState

channel :: Parsec String Int Channel  -- ()
channel = do char '<'
             spaces
             string "channel"
             spaces
             char '>'
             spaces
             modifyState (+1)
             tags <- many tag
             modifyState (subtract 1)
             spaces
             string "</"
             spaces
             string "channel"
             spaces
             char '>'
             Channel tags <$> getState

tag :: Parsec String Int Tag  -- ()
tag = do try (do char '<'
                 notFollowedBy (char '/'))
         spaces
         name <- many (letter <|> digit)
         spaces
         modifyState (+2)
         attrs <- many attribute
         modifyState (subtract 2)
         spaces
         close <- try (string "/>") <|> string ">"
         tabs <- getState
         spaces
         if length close == 2
            then return (SCTag name attrs tabs)
            else do modifyState (+1)
                    elemBody <- many elementBody
                    modifyState (subtract 1)
                    spaces
                    endTag name
                    spaces
                    return (Element name attrs elemBody tabs)

endTag :: String -> Parsec String Int Char  -- ()
endTag str = string "</" >> spaces >> string str >> spaces >> char '>'

elementBody :: Parsec String Int Tag  -- ()
elementBody = do spaces
                 try cData <|> try tag <|> text

text :: Parsec String Int Tag  -- ()
text = do txt <- many1 (noneOf "<>")
          Body txt <$> getState

cData :: Parsec String Int Tag  -- ()
cData = do string "<!"
           txt <- manyTill anyChar (string "]]>")
           Body ("<!" ++ txt ++ "]]>") <$> getState

main :: IO ()
main = do [input, output] <- getArgs
          h <- openFile input ReadMode
          hSetEncoding h latin1
          cnts <- hGetContents h
          case runParser document 1 input cnts of  -- parse
            Left err -> print err
            Right rss -> writeFile output . show $ rss  -- putStrLn . show $ rss
          hClose h
