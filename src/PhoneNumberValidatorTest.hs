{-# LANGUAGE OverloadedStrings #-}

import Data.PhoneNumber
import Control.Monad
import Data.Monoid
import qualified Data.ByteString.Char8 as S

main :: IO ()
main = forever $ do
    l <- S.getLine

    S.putStrLn "Number:"
    print $ parsePhoneNumber l "AU"
    print (refType <$> parsePhoneNumberRef l "AU")
    S.putStrLn $ "Characters keypad normalised: " <> convertAlphaCharacters l