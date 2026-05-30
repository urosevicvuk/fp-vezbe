{-  Parsec

    Instalacija
        - system-wide
            - cabal install parsec
        - sand box
            - mkdir asdf
            - cd asdf
            - cabal sandbox init
            - cabal install --only-dependencies
            - cabal install parsec
            - cabal repl
    
    Nalazi se u modulu Text.Parsec
    
    Literatura
        - http://book.realworldhaskell.org/read/using-parsec.html
        - http://dev.stephendiehl.com/fun/002_parsers.html
        - https://www.cnblogs.com/ncore/p/6892500.html
    
    Dokumentacija
        - http://hackage.haskell.org/package/parsec
    
    Upotreba
        - Definisemo gramatiku
        - Definisemo najsitnija pravila
        - Kombinacijom tih pravila sklapamo slozenija pravila sve do celog parsera
        - Taj parser prosledjujemo funkciji "parse" zajedno sa imenom file-a i
          tekstom za parsiranje
    
    Parsiranje se vrsi funkcijama "parse" i "runParser"
        - parse :: Stream s Data.Functor.Identity.Identity t => 
                   Parsec s () a -> SourceName -> s -> Either ParseError a
        - runParser :: Stream s Data.Functor.Identity.Identity t =>
                       Parsec s u a -> u -> SourceName -> s -> Either ParseError a

-}

import Text.Parsec

{- char i string

    Funkcije
        - char :: Stream s m Char => Char -> ParsecT s u m Char
        - string :: Stream s m Char => String -> ParsecT s u m String
    
    Uzimaju Char / String i vracaju pravilo za parsiranje navedenog Char-a / String-a

-}

-- primer char i string

-- parse (char 'p') "" "pera"

-- parse (char 'e') "" "pera"

-- parse (string "pera") "" "pera"

-- parse (string "per") "" "pera"

-- parse (string "pera") "" "per"

-- parse (string "pera") "" "petar"

{-  oneOf i noneOf

    Funkcije
        - oneOf :: Stream s m Char => [Char] -> ParsecT s u m Char
        - noneOf :: Stream s m Char => [Char] -> ParsecT s u m Char
    
    Uzimaju lilstu karaktera i vracaju pravilo za parsiranje bilo kojeg odnosno
    ni jednog karaktera iz liste
    
    Dodatne funkcije
        - anyChar :: Stream s m Char => ParsecT s u m Char
        - letter :: Stream s m Char => ParsecT s u m Char
        - lower :: Stream s m Char => ParsecT s u m Char
        - upper :: Stream s m Char => ParsecT s u m Char
        - digit :: Stream s m Char => ParsecT s u m Char
        - alphaNum :: Stream s m Char => ParsecT s u m Char
        - endOfLine :: Stream s m Char => ParsecT s u m Char
        - eof :: (Stream s m t, Show t) => ParsecT s u m ()
        - tab :: Stream s m Char => ParsecT s u m Char
        - space :: Stream s m Char => ParsecT s u m Char
        - spaces :: Stream s m Char => ParsecT s u m ()

-}

-- primer oneOf i noneOf

-- parse (oneOf "abcd") "" "abba"

-- parse (oneOf "abcd") "" "gabba"

-- parse (oneOf ['a'..'z']) "" "abba"

-- parse (noneOf ['a'..'z']) "" "abba"

-- parse (noneOf ['a'..'z']) "" "123"

{-  many i many1

    Funkcije
        - many :: ParsecT s u m a -> ParsecT s u m [a]
        - many1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]
    
    Uzimaju pravilo i ponavljaju to pravilo dokle god moze
    
    "many" vraca tacno cak i ako ne moze ni jednom da se primeni pravilo
    i vratice praznu listu
    
    "many1" vraca tacno samo ako pravilo moze bar jednom da se primeni u suprotnom
    vraca gresku

-}

-- primer many i many1

-- parse (many (char 'p')) "" "pppeeraa"

-- parse (many (char 'p')) "" "eeraa"

-- parse (many1 (char 'p')) "" "pppeeraa"

-- parse (many1 (char 'p')) "" "eeraa"

{-  count, manyTill i option

    Funkcije
        - count :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m [a]
        - manyTill :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
        - option :: Stream s m t => a -> ParsecT s u m a -> ParsecT s u m a
    
    "count" uzima broj i pravilo i ponavlja to pravilo zadati broj puta
    
    "manyTill" uzizma 2 pravila i ponavlja prvo 0 ili vise puta i ocekuje odmah nakon toga
    da ispuni drugo, a vraca gresku ako pukne neko od pravila ili naidje na nesto sto ne zadovoljava
    ni jedno ni drugo
        - vraca samo rezultat prvog pravila ali "trosi" i sve sto zadovolji drugo
    
    "option" uzima default vrednost i pravilo i vraca rezultat pravila ako uspe, a u suprotnom
    zadatu default vrednost
    
    Dodatne funkcije
        - between :: Stream s m t =>
                     ParsecT s u m open -> ParsecT s u m close ->
                     ParsecT s u m a -> ParsecT s u m a

-}

-- primer count, manyTill i option

-- parse (count 4 digit) "" "123456"

-- parse (count 4 digit) "" "123"

-- parse (count 4 digit) "" "pera"

-- parse (manyTill letter space) "" "pera 123"

-- parse (manyTill letter space) "" " 123"

-- parse (manyTill letter space) "" "pera123"

-- parse (option 'x' letter) "" "pera"

-- parse (option 'x' letter) "" "123"

-- parse (option "x" (string "per")) "" "pera"

-- parse (option "x" (string "pera")) "" "per"

{-  Kombinovanje pravila

    Parsec je monadic i vaze ista pravila kao kod drugih monada
    
    Kombinovanjem prostijih pravila pravimo slozenija
    
    Tip pravila mora da bude "Parsec <input_type> <state> <output_type>"

-}

-- primer Kombinovanje pravila

fon :: Parsec String () (String, String)
fon = do ime <- many1 letter
         spaces  -- many space -- many1 space
         br <- many1 digit
         return (ime, br)

fon' :: Parsec String () (String, String)
fon' = many1 letter >>= \ime -> spaces >> many1 digit >>= \br -> return (ime, br)

-- parse fon "" "pera 1234"

-- parse fon "" "pera  1234"

-- parse fon' "" "pera1234"

-- parse fon' "" "pera"

data Record = Record (String, String) deriving (Show)

fonR :: Parsec String () Record
fonR = do ime <- many1 letter
          spaces
          br <- many1 digit
          return (Record (ime, br))

separator :: Parsec String () ()
separator = spaces >> char ',' >> spaces

imenik :: Parsec String () [Record]
imenik = many $ do zapis <- fonR
                   separator
                   return zapis

imenik' :: Parsec String () [Record]
imenik' = many (fonR >>= \zapis -> separator >> return zapis)

-- parse imenik "" "pera 1234, mica  2234 ,neko98798   ,  zeljko 234,"

{-  endBy, sepBy i sepEndBy

    Funkcije
        - endBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
        - sepBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
        - sepEndBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
    
    "endBy" uzima 2 pravila parsira prvo i uvek ocekuje drugo posle prvog i vraca listu rezultata
    parsiranja prvim pravilom
    
    "sepBy" isto kao "endBy" ali ne sme da ispuni drugo pravilo na kraju
    
    "sepEndBy" isto kao "sepBy" ali moze i ne mora da ispuni drugo pravilo na kraju
    
    Dodatne funkcije
        - sepBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a] S
        - endBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
        - sepEndBy1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]

-}

-- primer endBy, sepBy i sepEndBy

endImenik :: Parsec String () [Record]
endImenik = endBy fonR separator

sepImenik :: Parsec String () [Record]
sepImenik = sepBy fonR separator

sepEndImenik :: Parsec String () [Record]
sepEndImenik = sepEndBy fonR separator

-- parse endImenik "" "pera 1234, mica  2234 ,neko98798   ,  zeljko 234,"

-- parse endImenik "" "pera 1234, mica  2234 ,neko98798   ,  zeljko 234"

-- parse sepImenik "" "pera 1234, mica  2234 ,neko98798   ,  zeljko 234"

-- parse sepImenik "" "pera 1234, mica  2234 ,neko98798   ,  zeljko 234,"

-- parse sepEndImenik "" "pera 1234, mica  2234 ,neko98798   ,  zeljko 234"

-- parse sepEndImenik "" "pera 1234, mica  2234 ,neko98798   ,  zeljko 234,"

{-  choice, <|> i try

    Funkcije
        - choice :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m a
        - (<|>) :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
        - try :: ParsecT s u m a -> ParsecT s u m a
    
    "choice" uzima listu pravila i pokusava da parsira redom kojim su pravila navedena
    i vraca rezultat prvog pravila koje uspe
    
    "<|>" isto kao "choice" samo povezuje 2 pravila ali se moze ulancavati
    
    "try" uzima pravilo i pokusava da ga izvrsi ali ako ne uspe premotava ulaz na stanje
    pre izvrsavanja navedenog pravila
    
    Dodatne funkcije
        - (<?>) :: ParsecT s u m a -> String -> ParsecT s u m a

-}

-- primer choice, <|> i try

-- parse (choice [string "pera", string "milan"]) "" "pera"

-- parse (choice [string "pera", string "milan"]) "" "milan"

-- parse (string "pera" <|> string "milan") "" "milan"

-- parse (string "pera" <|> string "milan" <?> "test") "" "bole"

-- parse (digit <?> "broj") "" "asd"

-- parse (string "pera" <|> string "petar") "" "petar"

-- parse (try (string "pera") <|> string "petar") "" "petar"

-- parse (choice [try (string "pera"), string "petar"]) "" "petar"

imenik'' :: Parsec String () [Record]
imenik'' = many $ do zapis <- fonR
                     choice [eof, separator]  -- eof <|> separator
                     return zapis

-- parse imenik'' "" "pera 1234, mica  2234 ,neko98798   ,  zeljko 234,"

-- parse imenik'' "" "pera 1234, mica  2234 ,neko98798   ,  zeljko 234"
