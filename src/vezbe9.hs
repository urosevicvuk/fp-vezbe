{-  newtype

    Kljucna rec "newtype" koristi se da se wrapp-uje postojeci tip u novi
        - Npr. ako zelimo da navedeni tip bude instanca neke klase na vise nacina
    
    Moze da se postigne isto sa "data" ali ovako radi brze
    
    Sme da ima samo jedan value konstruktor

-}

import qualified Control.Monad.Fail as Fail

-- primer newtype

data Piksel a = Color a a a | GScale a deriving (Show)

instance Functor Piksel where
    fmap f (GScale g) = GScale (f g)
    fmap f (Color r g b) = Color (f r) (f g) (f b)

newtype NewPiksel a = NewPiksel (Piksel a) deriving (Show)

instance Functor NewPiksel where
    fmap f (NewPiksel (GScale g)) = NewPiksel (GScale (f g))
    fmap f (NewPiksel (Color r g b)) = NewPiksel (Color (f b) (f r) (f g))

{-  Monad

    Type class
        - class Applicative m => Monad (m :: * -> *) where
              (>>=) :: m a -> (a -> m b) -> m b
              (>>) :: m a -> m b -> m b
              return :: a -> m a
              fail :: String -> m a  - U novijim verzijama jezika prebaceno u Control.Monad.Fail.MonadFail
              {-# MINIMAL (>>=) #-}
    
    Ideja je da se funkcija koja prima "cist" parametar i vraca vrednost u kontekstu
    primenjuje na podatak u kontekstu
    
    Funkcija ">>" se koristi kada nam ne treba vrednost levog parametra ali
    zelimo da sacuvamo kontekst
        - u "do" bloku se pise bez "<-"
    
    Funkcija "fail" se poziva automatski kada ne uspe pattern match u do bloku
    
    Ne navodi se parametar konkretnog tipa
    
    Ako type konstruktor prima 2 parametra tipa mora se parcijalno primeniti
    
    Kljucna rec "do" radi u kontekstu bilo kojeg Monada, ne samo IO i ponasa se isto
        - povezuje nekoliko operacija sa Monadima u blok
        - vrednost celog bloka je vrednost poslednjeg izraza u bloku
        - najcesce se koristi ako imamo ugnjezdene pozive ">>=" funkcije
        - takodje se moze koristiti operator "<-"
    
    Pravila
        - return x >>= f = f x
        - m >>= return = m
        - (m >>= f) >>= g = m >>= (\x -> f x >>= g)
    
    Primer
        - instance Monad Maybe where
              return x = Just x
              Nothing >>= f = Nothing
              Just x >>= f  = f x
              fail _ = Nothing
        
        - instance Monad [] where
              return x = [x]
              xs >>= f = concat (map f xs)
              fail _ = []

    Dodatna literatura:
        - https://wiki.haskell.org/All_About_Monads
	
-}

-- primer Monad

plus2 :: Int -> Maybe Int
plus2 x
    | rezultat > 10 = Nothing
    | otherwise = Just rezultat
    where rezultat = x + 2

minus1 :: Int -> Maybe Int
minus1 x
    | rezultat < 0 = Nothing
    | otherwise = Just rezultat
    where rezultat = x - 1

uslov :: Int -> Maybe Int
uslov x
    | even x = Nothing
    | otherwise = Just x

operacija :: Int -> Maybe Int
operacija x = do a <- plus2 x
                 b <- minus1 a
                 uslov b
                 c <- plus2 b
                 return c

vici :: String -> Maybe String
vici str = Just (str ++ "!")

test :: String -> Maybe String
test str = do (x:xs) <- vici str  -- y
              return xs

data Logger a = Logger { getLog :: (a, [String]) }

instance Functor Logger where
    fmap f (Logger (x, log)) = Logger (f x, log)

instance Applicative Logger where
    pure x = Logger (x, [])
    (Logger (f, log1)) <*> (Logger (x, log2)) = Logger (f x, log1 `mappend` log2)

instance Semigroup a => Semigroup (Logger a) where
    (Logger (x, log1)) <> (Logger (y, log2)) =  Logger (x <> y, log1 <> log2)

instance Monoid a => Monoid (Logger a) where
    mempty = Logger (mempty, [])

instance Monad Logger where
    return x = Logger (x, [])
    (Logger (x, log)) >>= f = Logger (res, log `mappend` newLog)
                                where (Logger (res, newLog)) = f x

instance Fail.MonadFail Logger where
    fail msg = error msg

funny :: String -> Logger String
funny str = Logger (str ++ "ha", ["inc"])

notFunny :: String -> Logger String
notFunny str
    | length str >= 2 = Logger (init . init $ str, ["dec"])
    | otherwise = fail "nema dovoljno"

smeh :: String -> Logger String
smeh str = do a <- funny str
              b <- notFunny a
              (x:y:xs) <- notFunny b
              return (y:x:xs)
