import Control.Monad
import Text.Parsec
import Text.Read


{-
    Napisati funkciju koja resava matematicku jednacinu u Reverse Polish Notation-u
    tako da radi i sa pogresnim unosom.
-}

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((x * y):ys)
foldingFunction (x:y:ys) "+" = return ((x + y):ys)
foldingFunction (x:y:ys) "-" = return ((y - x):ys)
foldingFunction xs numberString = fmap (:xs) (readMaybe numberString)
    where readMaybe st = case reads st of [(x,"")] -> Just x
                                          _ -> Nothing

solveRPN :: String -> Maybe Double
solveRPN st = do [result] <- foldM foldingFunction [] . words $ st
                 return result


{-
    Napisati funkciju koja resava matematicku jednacinu u Reverse Polish Notation-u
    tako da radi i sa pogresnim unosom i ispisuje poruku o gresci.
-}

foldingFunction' :: [Double] -> String -> Either String [Double]
foldingFunction' (x:y:ys) "*" = return ((x * y):ys)
foldingFunction' (x:y:ys) "+" = return ((x + y):ys)
foldingFunction' (x:y:ys) "-" = return ((y - x):ys)
foldingFunction' xs numberString = fmap (:xs) (readMaybe numberString)
    where readMaybe st = case reads st of [(x,"")] -> Right x
                                          _ -> Left "Pogresan unos"

solveRPN' :: String -> Either String Double
solveRPN' st = do result <- foldM foldingFunction' [] . words $ st
                  case length result of 1 -> Right . head $ result
                                        _ -> Left "Nije izracunato do kraja"

{-
    Definisati tip binarno stablo i instancirati klasu Functor.
-}

data Stablo a = Prazno | Cvor a (Stablo a) (Stablo a) deriving Show

instance Functor Stablo where
    fmap f Prazno = Prazno
    fmap f (Cvor x l d) = Cvor (f x) (fmap f l) (fmap f d)

{-
    Definisati tip Stack i instancirati klase Functor, Applicative, Monoid i Monad.
-}

data Stack a = Prazan | Stack [a] deriving Show

instance Functor Stack where
    fmap f Prazan = Prazan
    fmap f (Stack xs) = Stack (map f xs)

instance Monoid (Stack a) where
    mempty = Prazan

instance Semigroup (Stack a) where
    Prazan <> st = st
    st <> Prazan = st
    (Stack xs) <> (Stack ys) = Stack (xs `mappend` ys)

instance Applicative Stack where
    pure x = Stack [x]
    Prazan <*> _ = Prazan
    (Stack fs) <*> (Stack xs) = Stack [f x | f <- fs, x <- xs]

instance Monad Stack where
    return = pure
    Prazan >>= _ = Prazan
    Stack xs >>= f = foldl mappend Prazan (fmap f xs)

plusSt :: Int -> Stack Int
plusSt x = Stack [x + 1]

{-
    Definisati tip kompleksnih brojeva i instancirati klase Show, Functor, Applicative i Monad.
-}

data Complex a = Complex a a

instance (Show a, Num a, Ord a) => Show (Complex a) where
    show (Complex x y) = show x ++ (if y < 0 then "-" else "+") ++ show (abs y) ++ "i"

instance Functor Complex where
    fmap f (Complex x y) = Complex (f x) (f y)

instance Applicative Complex where
    pure x = Complex x x
    (Complex f g) <*> (Complex x y) = Complex (f x) (g y)

instance Monad Complex where
    return = pure
    (Complex x y) >>= f = let (Complex a b) = f x; (Complex c d) = f y in Complex a d

{-
    Napisati parser za sabiranje i oduzimanje kompleksnih brojeva.
-}

number :: Parsec String () String
number = many1 digit

minus :: Parsec String () String
minus = (:) <$> char '-' <*> (spaces >> number)

plus :: Parsec String () String
plus = char '+' >> spaces >> number

rd :: String -> Maybe Double
rd = readMaybe

complexB :: Parsec String () (Complex Double)
complexB = do spaces
              r <- rd <$> do num <- plus <|> minus <|> number
                             spaces
                             notFollowedBy (char 'i')
                             return num
              i <- rd <$> do num <- plus <|> minus <|> number
                             spaces
                             char 'i'
                             spaces
                             return num
              case r of Nothing -> case i of Nothing -> return (Complex 0 0)
                                             Just y -> return (Complex 0 y)
                        Just x -> case i of Nothing -> return (Complex x 0)
                                            Just y -> return (Complex x y)

complexR :: Parsec String () (Complex Double)
complexR = do spaces
              r <- rd <$> do num <- plus <|> minus <|> number
                             spaces
                             notFollowedBy (char 'i')
                             return num
              case r of Nothing -> return (Complex 0 0)
                        Just x -> return (Complex x 0)

complexI :: Parsec String () (Complex Double)
complexI = do spaces
              i <- rd <$> do num <- plus <|> minus <|> number
                             spaces
                             char 'i'
                             spaces
                             return num
              case i of Nothing -> return (Complex 0 0)
                        Just x -> return (Complex 0 x)

complex :: Parsec String () (Complex Double)
complex = try complexB <|> try complexR <|> complexI

operation :: Num a => Parsec String () (a->a->a)
operation = do spaces
               op <- oneOf "+-"
               case op of '+' -> return (+)
                          '-' -> return (-)

eq :: Parsec String () (Complex Double)
eq = do spaces
        c1 <- complex
        spaces
        op <- operation
        spaces
        c2 <- complex
        return $ op <$> c1 <*> c2

calc :: Parsec String () (Complex Double)
calc = try eq <|> complex

{-
    Napisati program koji sa komandne linije ucitava jednacine sa kompleksnim brojevima
    i ispisuje rezultat. Program se zavrsava unosom prazne linije.
-}

main = do line <- getLine
          if null line then return ()
                       else do case parse calc "" line of Right x -> print x
                                                          Left err -> print err
                               main
