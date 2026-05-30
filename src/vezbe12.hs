{-  Randomness

    Haskell kao cisto funkcionalni jezik postuje "referential transparency"
        - funkcija pozvana sa istim parametrom ce uvek vratiti isti rezultat
        - ovo je problem ako nam treba randon vrednost
	
	Instalacija
        - cabal update
        - cabal install --lib random
    
    Modul System.Random eksportuje funkcije koje se koriste za generisanje random vrednosti
        - random :: (RandomGen g, Random a) => g -> (a, g)
        - randoms :: (RandomGen g, Random a) => g -> [a]
        - randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
        - randomRs :: (RandomGen g, Random a) => (a, a) -> g -> [a]
        - mkStdGen :: Int -> StdGen
        - getStdGen :: IO StdGen
        - newStdGen :: IO StdGen
    
    Upotrebom funkcije mkStdGen generisemo podatak tipa StdGen koji je instanca klase RandomGen
        - ovu vrednost koristimo da generisem trazenu random vrednost
        - najpribliznije seed vrednosti kod klasicnih jezika
    
    Moze se koristiti u kombinaciji sa State monadom

-}

import System.Random
import System.Environment
import System.IO
import System.IO.Error
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

-- random (mkStdGen 5) :: (Int, StdGen)

-- random (mkStdGen 5) :: (Bool, StdGen)

twoBools :: Int -> (Bool, Bool)
twoBools x = (first, second)
    where (first, fstGen) = random gen
          (second, sndGen) = random fstGen
          gen = mkStdGen x

-- take 2 $ randoms (mkStdGen 4) :: [Bool]

-- randomR (1,6) (mkStdGen 4)

-- take 3 . randomRs ('a','z') . mkStdGen $ 4 :: [Char]

{-
    main = do gen <- getStdGen
              putStrLn . take 5 . randomRs ('A', 'Z') $ gen
              gen' <- newStdGen
              putStrLn . take 5 . randomRs ('A', 'Z') $ gen'
-}

{-  ByteString

    Kao String ali je svaki element liste tacno jedan byte (Word8)
    
    Imaju bolje performanse nego obicni string-ovi
    
    Po default-u nisu lazy
        - ne mogu da budu beskonacni
        - ako ih koristimo za citanje file-a ucitavaju ceo sadrzaj u memoriju
    
    Postoje 2 tipa
        - strict u modulu Data.ByteString
        - lazy u modulu Data.ByteString.Lazy
    
    Lazy rade slicno kao liste ali u chunk-ovima po 64 KB
        - pogodniji za rad sa velikim file-ovima
    
    Prazan ByteString je oznacen konstantom "empty"
    
    Vecina funkcija iz Data.List modula ima ekvivalenta u Data.ByteString modulu
	
	Instalacija
        - cabal update
        - cabal install --lib bytestring
    
    Korisne funkcije
        - pack :: [GHC.Word.Word8] -> ByteString
        - unpack :: ByteString -> [GHC.Word.Word8]
        - cons :: GHC.Word.Word8 -> ByteString -> ByteString
        - cons' :: GHC.Word.Word8 -> ByteString -> ByteString
        - readFile :: FilePath -> IO ByteString
        - writeFile :: FilePath -> ByteString -> IO ()

-}

-- :m + Data.ByteString.Lazy

-- pack [45,46,48]

-- unpack (pack [45,46,48])

-- cons 85 $ empty

-- cons 85 $ pack [45,46,48]

{-  Exceptions

    U modulu Control.Exception se nalaze funkcije za rad sa izuzecima
    
    U modulu System.IO.Error se nalaze funkcije za rad sa I/O izuzecima
    
    U cisto funkcionalnom delu koda UVEK koristiti ranije opisane mehanizme za
    rad sa greskama
    
    Exception-e koristiti SAMO u IO delu koda
    
    Funkcije
        - catch :: Exception e => IO a -> (e -> IO a) -> IO a
        - ioError :: IOError -> IO a
        - userError :: String -> IOError
        - ioeGetFileName :: IOError -> Maybe FilePath
    
    Predikati
        - isAlreadyExistsError
        - isDoesNotExistError
        - isAlreadyInUseError
        - isFullError
        - isEOFError
        - isIllegalOperation
        - isPermissionError
        - isUserError
    
    Detaljna lista svih funkcija moze se naci u dokumentaciji
        - http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html
        - http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO-Error.html

-}

main :: IO ()
main = catch probaj handler''

probaj :: IO ()
probaj = do [file] <- getArgs
            cont <- readFile file
            putStrLn $ "Linija: " ++ (show . length . lines $ cont)

handler :: IOError -> IO ()
handler e = putStrLn "Greska!"

handler' :: IOError -> IO ()
handler' e
    | isDoesNotExistError e = putStrLn "Nema!"
    | otherwise = ioError e

handler'' :: IOError -> IO ()
handler'' e
    | isDoesNotExistError e = case ioeGetFileName e of Just path -> putStrLn $ "Nema: " ++ path
                                                       Nothing -> putStrLn "Nepoznata lokacija!"
    | otherwise = ioError e

{-  Zippers

    Nacin kretanja kroz slozene strukture podataka
        - moze se posmatrati kao iterator
    
    Ideja je da se wrapp-uje zeljena struktura u novi tip koji prati trenutnu poziciju
    i ostatak strukture
        - voditi racuna da se ne narusi struktura koja se obilazi

-}

data Stablo a = Prazno | Cvor a (Stablo a) (Stablo a) deriving (Show)
data Smer a = Levo a (Stablo a) | Desno a (Stablo a) deriving (Show)

type Put a = [Smer a]
type Zipper a = (Stablo a, Put a)

tmp :: Stablo Int
tmp = Cvor 1
        (Cvor 2
            (Cvor 3 Prazno Prazno)
            (Cvor 4 Prazno Prazno))
        (Cvor 5
            (Cvor 6 Prazno Prazno)
            (Cvor 7 Prazno Prazno))

zpr :: Zipper Int
zpr = (tmp, [])

levo :: Zipper a -> Zipper a
levo (Cvor x l r, put) = (l, Levo x r : put)

desno :: Zipper a -> Zipper a
desno (Cvor x l r, put) = (r, Desno x l : put)

gore :: Zipper a -> Zipper a
gore (l, Levo x r : put) = (Cvor x l r, put)
gore (r, Desno x l : put) = (Cvor x l r, put)

izmeni :: (a -> a) -> Zipper a -> Zipper a
izmeni _ (Prazno, _) = error "Nema cvorova!"
izmeni f (Cvor x l r, put) = (Cvor (f x) l r, put)


{-  Monad Transformers

    Sluze da kombinuju funkcionalnosti 2 Monada u jedna tip
    
    Primer
        - newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
        
        - instance Monad m => Monad (MaybeT m) where
              return  = MaybeT . return . Just

              -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
              x >>= f = MaybeT $ do maybe_value <- runMaybeT x
                                    case maybe_value of
                                        Nothing    -> return Nothing
                                        Just value -> runMaybeT $ f value

    Dodatna literatura:
        - https://en.wikibooks.org/wiki/Haskell/Monad_transformers

-}

getWord :: IO (Maybe String)
getWord = do word <- getLine
             if 'a' `elem` word
                then return (Just word)
                else return Nothing

askWord :: IO ()
askWord = do putStrLn "Unesite rec sa slovom 'a':"
             result <- getWord
             case result of
                 Nothing -> putStrLn "Rec ne sadrzi slovo 'a'!"
                 Just word -> putStrLn $ "Uneli ste: " ++ word

getWordT :: MaybeT IO String
getWordT = do word <- lift getLine
              guard ('a' `elem` word)
              return word

askWordT :: MaybeT IO ()
askWordT = do lift . putStrLn $ "Unesite rec sa slovom 'a':"
              result <- getWordT
              lift . putStrLn $ "Uneli ste: " ++ result

