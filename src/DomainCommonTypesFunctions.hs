module DomainCommonTypesFunctions where

import Data.Monoid


-- CONSTRAINED TYPES


type ErrorMesage = String

createString :: String 
                -> (String -> String) 
                -> Int 
                -> String 
                -> Either ErrorMesage String
createString fieldName ctor maxLen str  
    | null str =
        let errorMsg =  
                fieldName 
                <> " must not be null or empty" 
        in Left errorMsg
    | length str > maxLen =
        let errorMsg = 
                fieldName 
                <> " must not be more than "
                <> show maxLen  
                <> " chars"
        in Left errorMsg
    | otherwise =
        Right $ ctor str

createStringControlledLength :: String 
                -> (String -> String) 
                -> Int 
                -> Int
                -> String 
                -> Either ErrorMesage String
createStringControlledLength fieldName ctor minLen maxLen str       
    | minLen >= maxLen = 
        let errorMsg = 
                "incoherent max and min length"
        in Left errorMsg
    | null str =
        let errorMsg = 
                fieldName 
                <> " must not be null or empty"  
        in Left errorMsg
    | length str > maxLen =
        let errorMsg = 
                fieldName 
                <> " must not be more than " 
                <> show maxLen 
                <> " chars" 
        in Left  errorMsg 
    | length str < minLen =
        let errorMsg = 
                fieldName 
                <> " must not be less than " 
                <> show minLen <> " chars"   
        in Left errorMsg 
    | otherwise =
        Right $ ctor str


  
            
createStringOption :: String 
                -> (String -> String) 
                -> Int 
                -> String 
                -> Either ErrorMesage (Maybe String)
createStringOption fieldName ctor maxLen str
    | null str =
        Right Nothing
    | length str > maxLen =
        let errorMsg = fieldName <> " must not be more than " <> show maxLen <> " chars"   
        in Left errorMsg 
    | otherwise =
        Right $ Just $ ctor str 


{--

createInt fieldName ctor minVal maxVal i
    | i < minVal =
        let msg = sprintf "%s: Must not be less than %i" fieldName minVal
        Error msg
    elif i > maxVal then
        let msg = sprintf "%s: Must not be greater than %i" fieldName maxVal
        Error msg
    else
        Ok (ctor i)




    
createDecimal fieldName ctor minVal maxVal i = 
    if i < minVal then
        let msg = sprintf "%s: Must not be less than %M" fieldName minVal
        Error msg
    elif i > maxVal then
        let msg = sprintf "%s: Must not be greater than %M" fieldName maxVal
        Error msg
    else
        Ok (ctor i)




createLike fieldName  ctor pattern str = 
    if String.IsNullOrEmpty(str) then
        let msg = sprintf "%s: Must not be null or empty" fieldName 
        Error msg
    elif System.Text.RegularExpressions.Regex.IsMatch(str,pattern) then
        Ok (ctor str)
    else
        let msg = sprintf "%s: '%s' must match the pattern '%s'" fieldName str pattern
        Error msg 
--}