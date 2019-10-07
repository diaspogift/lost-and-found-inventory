module DomainCommonTypes (
      LostItemId, FoundItemId
    , MatchedItemId, ClaimedItemId
    , UserId, TenantId
    , CategoryId, ShortDescription, LongDescription
    , Name, City, Village, Neighborhood, Address
    , AttributeCode, AttributeName, AttributeValue, AttributeUnit
    , EmailAddress, PostalAddress, Telephone
    , FirstName, Middle, LastName
    , Question, Answer
    , createFoundItemId, createLostItemId
    , cretateMatchedItemId, createClaimedItemId
    , createUserId, createTenantId, createCategoryId
    , createShortDescription, createLongDescription
    , createName, createCity, createVillage, createNeighborhood, createAddress
    , createAttributeCode, createAttributeName, createAttributeValue, createAttributeUnit
    , createEmailAddress, createPostalAddress, createTelephone
    , createFirstName, createMiddle, createLastName
    , createQuestion, createAnswer
    ) where 


import Data.Monoid
import qualified Text.Email.Validate as EmailVal
import qualified Data.ByteString.Char8 as Char8


--- Common types wrappers
--- 
---

newtype LostItemId = 
    LostItemId String deriving (Eq, Ord, Show)
newtype FoundItemId = 
    FoundItemId String deriving (Eq, Ord, Show)
newtype MatchedItemId = 
    MatchedItemId String deriving (Eq, Ord, Show)
newtype ClaimedItemId = 
    ClaimedItemId String deriving (Eq, Ord, Show)
newtype UserId = 
    UserId String deriving (Eq, Ord, Show)
newtype TenantId = 
    TenantId String deriving (Eq, Ord, Show)
newtype CategoryId = 
    CategoryId String deriving (Eq, Ord, Show)
newtype Name = 
    Name String deriving (Eq, Ord, Show)
newtype CategoryCode =
    CategoryCode String deriving (Eq, Ord, Show)
newtype CategoryName =
    CategoryName String deriving (Eq, Ord, Show)
newtype CategoryDescription =
    CategoryDescription String deriving (Eq, Ord, Show)
newtype City = 
    City String deriving (Eq, Ord, Show)
newtype Village = 
    Village String deriving (Eq, Ord, Show)
newtype Neighborhood = 
    Neighborhood String deriving (Eq, Ord, Show)
newtype Address = 
     Address String deriving (Eq, Ord, Show)
newtype ShortDescription = 
    ShortDescription String deriving (Eq, Ord, Show)
newtype LongDescription = 
    LongDescription String deriving (Eq, Ord, Show)
newtype FirstName = 
    FirstName String deriving (Eq, Ord, Show)
newtype Middle = 
    Middle String deriving (Eq, Ord, Show)
newtype LastName = 
    LastName String deriving (Eq, Ord, Show)
newtype EmailAddress = 
    EmailAddress String deriving (Eq, Ord, Show)
newtype PostalAddress = 
    PostalAddress String deriving (Eq, Ord, Show)
newtype Telephone = 
    Telephone String deriving (Eq, Ord, Show)
newtype AttributeCode = 
    AttributeCode String deriving (Eq, Ord, Show)
newtype AttributeName = 
    AttributeName String deriving (Eq, Ord, Show)
newtype AttributeValue = 
    AttributeValue String deriving (Eq, Ord, Show)
newtype AttributeUnit = 
    AttributeUnit String deriving (Eq, Ord, Show)
newtype Question = 
    Question String deriving (Eq, Ord, Show)
newtype Answer = 
    Answer String deriving (Eq, Ord, Show)
type ErrorMessage = String



--- Common types constraints
--- 
---

createString :: String 
                -> (String -> a) 
                -> Int 
                -> String 
                -> Either ErrorMessage a
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
                -> (String -> a) 
                -> Int 
                -> Int
                -> String 
                -> Either ErrorMessage a
createStringControlledLength fieldName ctor minLen maxLen str       
    | minLen > maxLen = 
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
                -> (String -> a) 
                -> Int 
                -> String 
                -> Either ErrorMessage (Maybe a)
createStringOption fieldName ctor maxLen str
    | null str =
        Right Nothing
    | length str > maxLen =
        let errorMsg = fieldName <> " must not be more than " <> show maxLen <> " chars"   
        in Left errorMsg 
    | otherwise =
        Right $ Just $ ctor str 


createNum :: (Num a, Eq a, Ord a, Show a) =>
               String 
            -> (a -> a) 
            -> a 
            -> a
            -> a 
            -> Either ErrorMessage a
createNum fieldName ctor minVal maxVal i
    | minVal > maxVal =
        let errorMsg = "inconsitent minval and maxval"
        in Left errorMsg
    | i < minVal =
        let errorMsg =  
                fieldName 
                <> " : Must not be less than " 
                <> show minVal  
        in Left errorMsg
    | i > maxVal =
        let errorMsg = 
                fieldName 
                <> " : Must not be greater than " 
                <> show maxVal
        in Left errorMsg
    | otherwise =
        Right (ctor i)


createEmail :: String 
            -> (String -> EmailAddress) 
            -> String 
            -> Either ErrorMessage EmailAddress
createEmail fieldName  ctor  str 
    | null str =
        let errorMsg = 
                fieldName 
                <> ": Must not be null or empty"  
        in Left errorMsg
    | otherwise = 
        let email = EmailVal.validate . Char8.pack $ str
        in case email of 
                Right emailAddress -> Right $ ctor str
                Left errorMsg -> Left errorMsg



--- Common types constructors
--- 
---

createLostItemId :: String -> Either ErrorMessage LostItemId
createLostItemId = 
    createStringControlledLength "Lost Item Identifier: " LostItemId 36 36 

createFoundItemId :: String -> Either ErrorMessage FoundItemId
createFoundItemId = 
    createStringControlledLength "Found Item Identifier: " FoundItemId 36 36

cretateMatchedItemId :: String -> Either ErrorMessage MatchedItemId
cretateMatchedItemId = 
    createStringControlledLength "Matched Item Identifier: " MatchedItemId 36 36

createClaimedItemId :: String -> Either ErrorMessage ClaimedItemId
createClaimedItemId = 
    createStringControlledLength "Claimed Item Identifier: " ClaimedItemId 36 36


createUserId :: String -> Either ErrorMessage UserId
createUserId = 
    createStringControlledLength "User Identifier: " UserId 36 36

createTenantId :: String -> Either ErrorMessage TenantId
createTenantId = 
    createStringControlledLength "Tenant Identifier: " TenantId 36 36

createCategoryId :: String -> Either ErrorMessage CategoryId
createCategoryId = 
    createStringControlledLength "Category Identifier: " CategoryId 36 36



createShortDescription :: String -> Either ErrorMessage ShortDescription
createShortDescription = 
    createString "Short Description: " ShortDescription 250

createLongDescription :: String -> Either ErrorMessage LongDescription
createLongDescription = 
    createString "Long Description: " LongDescription 5000

    
createName :: String -> Either ErrorMessage Name
createName = 
    createString "Name: " Name 100

createCity :: String -> Either ErrorMessage City
createCity = 
    createString "City: " City 100

createVillage :: String -> Either ErrorMessage Village
createVillage = 
    createString "Village: " Village 100

createNeighborhood :: String -> Either ErrorMessage Neighborhood
createNeighborhood = 
    createString "Neighborhood: " Neighborhood 500

createAddress :: String -> Either ErrorMessage Address
createAddress = 
    createString "Address: " Address 500

createAttributeCode :: String -> Either ErrorMessage AttributeCode
createAttributeCode = 
    createString "Attribute Code: " AttributeCode 50

createAttributeName :: String -> Either ErrorMessage AttributeName
createAttributeName = 
    createString "Attribute Code: " AttributeName 50

createAttributeValue :: String -> Either ErrorMessage AttributeValue
createAttributeValue = 
    createString "Attribute Value: " AttributeValue 50

createAttributeUnit :: String -> Either ErrorMessage AttributeUnit
createAttributeUnit = 
    createString "Attribute Unit: " AttributeUnit 50

createEmailAddress :: String -> Either ErrorMessage EmailAddress
createEmailAddress = 
    createEmail "Email Address : " EmailAddress

createPostalAddress :: String -> Either ErrorMessage PostalAddress
createPostalAddress = 
    createString "Postal Address: " PostalAddress 500

createTelephone :: String -> Either ErrorMessage Telephone
createTelephone = 
    createString "Telephone: " Telephone 50   
    
createFirstName :: String -> Either ErrorMessage FirstName
createFirstName = 
    createString "First Name : " FirstName 100

createMiddle :: String -> Either ErrorMessage (Maybe Middle)
createMiddle = 
    createStringOption "Middle Name : " Middle 100

createLastName :: String -> Either ErrorMessage LastName
createLastName = 
    createString "Last Name : " LastName 100

createQuestion :: String -> Either ErrorMessage Question
createQuestion = 
    createString "Question : " Question 1000

createAnswer :: String -> Either ErrorMessage Answer
createAnswer = 
    createString "Answer : " Answer 1000












