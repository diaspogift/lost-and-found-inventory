{-# LANGUAGE TypeFamilies #-}

import Data.Function (on)
import Control.Applicative

data EventData e = EventData {
    eventId :: Int,
    body :: Event e
}

instance Show (EventData e) where
    show = show . eventId

instance Eq (EventData e) where
    (==) = (==) `on` eventId

instance Ord (EventData e) where
    compare = compare `on` eventId

class Aggregate s where
    data Error s :: *
    data Command s :: *
    data Event s :: *

    execute :: s -> Command s -> Either (Error s) (Event s)
    apply :: s -> Event s -> s
    seed :: s



data User = User {
    name :: String,
    email :: String
} deriving (Show)

instance Aggregate User where

    data Error User = NotAllowed
                    | TooShortUsername Int Int
                    | EmptyUsername
                    | EmptyEmail
                    deriving (Show)

    data Event User = NameChanged String 
                    | EmailChanged String 
                    deriving (Show)
    
    data Command User = ChangeName String 
                      | ChangeEmail String
                      deriving (Show) 

    _ `execute` ChangeName n = NameChanged 
        <$> validate notEmpty EmptyUsername n
        <* validate (lengthBetween 4 8) (TooShortUsername 4 8) n

    _ `execute` ChangeEmail e = EmailChanged 
        <$> validate notEmpty EmptyEmail e

    state `apply` NameChanged n = state { name = n }
    state `apply` EmailChanged e = state { email = e }

    seed = User "" ""

load :: (Aggregate a) => [EventData a] -> a
load = foldl folder seed
    where
        folder state = apply state . body

validate :: (a -> Bool) -> e -> a -> Either e a
validate f err x
    | f x = Right x
    | otherwise = Left err

notEmpty :: [a] -> Bool
notEmpty = (> 0) . length

lengthBetween :: Int -> Int -> String -> Bool
lengthBetween s e str
    | len >= s && len <= e = True
    | otherwise = False
    where len = length str

main :: IO()
main = do

    print $ load $ map (EventData 1) [NameChanged "Borak", NameChanged "Fristi", EmailChanged "email@gmail.com"]
    print $ execute seed $ ChangeEmail "email@gmail.com"
    print $ execute seed $ ChangeName "Te"