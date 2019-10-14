module CommonSimpleTypes (
      LostItemId, FoundItemId, DateTimeSpan
    , MatchedItemId, ClaimedItemId
    , UserId, TenantId
    , RegionName, DivisionName, SubDivisionName
    , CategoryId, ParentCategoryId, ShortDescription, LongDescription
    , TenantName, ItemName, City, Village, Neighborhood, Address
    , AttributeCode, AttributeName, AttributeValue, AttributeUnit
    , EmailAddress, PostalAddress, Telephone
    , FirstName, Middle, LastName
    , Question, Answer, ErrorMessage
    , createFoundItemId, createLostItemId
    , createMatchedItemId, createClaimedItemId
    , createUserId, createTenantId, createCategoryId
    , createShortDescription, createLongDescription
    , createTenantName, createItemName, createCity, createVillage, createNeighborhood, createAddress
    , createAttributeCode, createAttributeName, createAttributeValue, createAttributeUnit
    , createEmailAddress, createPostalAddress, createTelephone
    , createFirstName, createMiddle, createLastName
    , createQuestion, createAnswer, creatDateTimeSpan
    
    , unwrapFoundItemId, unwrapLostItemId
    , unwrapMatchedItemId, unwrapClaimedItemId
    , unwrapUserId, unwrapTenantId, unwrapCategoryId
    , unwrapShortDescription, unwrapLongDescription
    , unwrapTenantName, unwrapItemName, unwrapCity, unwrapVillage, unwrapNeighborhood, unwrapAddress
    , unwrapAttributeCode, unwrapAttributeName, unwrapAttributeValue, unwrapAttributeUnit
    , unwrapEmailAddress, unwrapPostalAddress, unwrapTelephone
    , unwrapFirstName, unwrapMiddle, unwrapLastName
    , unwrapQuestion, unwrapAnswer
    ) where 


import Data.Monoid
import qualified Text.Email.Validate as EmailVal
import qualified Data.ByteString.Char8 as Char8
import Data.Dates
import Data.List.Split






-- ===================================================================================
-- Simple types and constrained types related to the Lost |&| Found Inventory domain.
--
-- E.g. Single case discriminated unions (aka wrappers), enums, etc
-- ===================================================================================




type Country = String
type RegionName =  String
    --RegionName String deriving (Eq, Ord, Show)
type DivisionName = String
    --DivisionName String deriving (Eq, Ord, Show)
type SubDivisionName = String
    --SubDivisionName String deriving (Eq, Ord, Show)
newtype DateTimeSpan = 
    DateTimeSpan (DateTime, DateTime) deriving (Eq, Ord, Show)
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
type ParentCategoryId = 
    CategoryId
newtype TenantName = 
    TenantName String deriving (Eq, Ord, Show)
newtype ItemName = 
    ItemName String deriving (Eq, Ord, Show)
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
type ErrorMessage 
    = String






-- ===================================================================================
-- Reusable constructors and getters for constrained types
-- Reusable unwrappers to primitives types such as String, Int ..etc
-- ===================================================================================





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


createLostItemId :: String -> Either ErrorMessage LostItemId
createLostItemId = 
    createStringControlledLength "Lost Item Identifier: " LostItemId 36 36 

unwrapLostItemId :: LostItemId -> String
unwrapLostItemId (LostItemId str) = str

createFoundItemId :: String -> Either ErrorMessage FoundItemId
createFoundItemId = 
    createStringControlledLength "Found Item Identifier: " FoundItemId 36 36

unwrapFoundItemId :: FoundItemId -> String
unwrapFoundItemId (FoundItemId str) = str

createMatchedItemId :: String -> Either ErrorMessage MatchedItemId
createMatchedItemId = 
    createStringControlledLength "Matched Item Identifier: " MatchedItemId 36 36

unwrapMatchedItemId :: MatchedItemId -> String
unwrapMatchedItemId (MatchedItemId str) = str

createClaimedItemId :: String -> Either ErrorMessage ClaimedItemId
createClaimedItemId = 
    createStringControlledLength "Claimed Item Identifier: " ClaimedItemId 36 36

unwrapClaimedItemId :: ClaimedItemId -> String
unwrapClaimedItemId (ClaimedItemId str) = str

createUserId :: String -> Either ErrorMessage UserId
createUserId = 
    createStringControlledLength "User Identifier: " UserId 36 36

unwrapUserId :: UserId -> String
unwrapUserId (UserId str) = str

createTenantId :: String -> Either ErrorMessage TenantId
createTenantId = 
    createStringControlledLength "Tenant Identifier: " TenantId 36 36

unwrapTenantId :: TenantId -> String
unwrapTenantId (TenantId str) = str


createCategoryId :: String -> Either ErrorMessage CategoryId
createCategoryId = 
    createStringControlledLength "Category Identifier: " CategoryId 36 36

unwrapCategoryId :: CategoryId -> String
unwrapCategoryId (CategoryId str) = str

createShortDescription :: String -> Either ErrorMessage ShortDescription
createShortDescription = 
    createString "Short Description: " ShortDescription 250

unwrapShortDescription:: ShortDescription -> String
unwrapShortDescription (ShortDescription str) = str

createLongDescription :: String -> Either ErrorMessage LongDescription
createLongDescription = 
    createString "Long Description: " LongDescription 5000

unwrapLongDescription:: LongDescription -> String
unwrapLongDescription (LongDescription str) = str
    
createTenantName :: String -> Either ErrorMessage TenantName
createTenantName = 
    createString "Tenant Name: " TenantName 100

unwrapTenantName :: TenantName -> String
unwrapTenantName (TenantName str) = str

createItemName :: String -> Either ErrorMessage ItemName
createItemName = 
    createString "Item Name: " ItemName 500

unwrapItemName :: ItemName -> String
unwrapItemName (ItemName str) = str

createCity :: String -> Either ErrorMessage City
createCity = 
    createString "City: " City 100

unwrapCity:: City -> String
unwrapCity (City str) = str

createVillage :: String -> Either ErrorMessage Village
createVillage = 
    createString "Village: " Village 100

unwrapVillage :: Village -> String
unwrapVillage (Village str) = str

createNeighborhood :: String -> Either ErrorMessage Neighborhood
createNeighborhood = 
    createString "Neighborhood: " Neighborhood 500

unwrapNeighborhood :: Neighborhood -> String
unwrapNeighborhood (Neighborhood str) = str

createAddress :: String -> Either ErrorMessage Address
createAddress = 
    createString "Address: " Address 500

unwrapAddress :: Address -> String
unwrapAddress (Address str) = str

createAttributeCode :: String -> Either ErrorMessage AttributeCode
createAttributeCode = 
    createString "Attribute Code: " AttributeCode 50

unwrapAttributeCode :: AttributeCode -> String
unwrapAttributeCode (AttributeCode str) = str

createAttributeName :: String -> Either ErrorMessage AttributeName
createAttributeName = 
    createString "Attribute Code: " AttributeName 50

unwrapAttributeName :: AttributeName -> String
unwrapAttributeName (AttributeName str) = str

createAttributeValue :: String -> Either ErrorMessage AttributeValue
createAttributeValue = 
    createString "Attribute Value: " AttributeValue 50

unwrapAttributeValue :: Maybe AttributeValue -> String
unwrapAttributeValue (Just (AttributeValue str)) = str
unwrapAttributeValue Nothing = ""

createAttributeUnit :: String -> Either ErrorMessage AttributeUnit
createAttributeUnit = 
    createString "Attribute Unit: " AttributeUnit 50

unwrapAttributeUnit :: Maybe AttributeUnit -> String
unwrapAttributeUnit (Just (AttributeUnit str)) = str
unwrapAttributeUnit Nothing = ""

createEmailAddress :: String -> Either ErrorMessage EmailAddress
createEmailAddress = 
    createEmail "Email Address : " EmailAddress

unwrapEmailAddress :: EmailAddress -> String
unwrapEmailAddress (EmailAddress str) = str

createPostalAddress :: String -> Either ErrorMessage PostalAddress
createPostalAddress = 
    createString "Postal Address: " PostalAddress 500

unwrapPostalAddress :: PostalAddress -> String
unwrapPostalAddress (PostalAddress str) = str

createTelephone :: String -> Either ErrorMessage Telephone
createTelephone = 
    createString "Telephone: " Telephone 50  
    
unwrapTelephone :: Telephone -> String
unwrapTelephone (Telephone str) = str
    
createFirstName :: String -> Either ErrorMessage FirstName
createFirstName = 
    createString "First Name : " FirstName 100

unwrapFirstName:: FirstName -> String
unwrapFirstName (FirstName str) = str

createMiddle :: String -> Either ErrorMessage (Maybe Middle)
createMiddle = 
    createStringOption "Middle Name : " Middle 100

unwrapMiddle :: Maybe Middle -> String
unwrapMiddle (Just (Middle str)) = str
unwrapMiddle Nothing = ""

createLastName :: String -> Either ErrorMessage LastName
createLastName = 
    createString "Last Name : " LastName 100

unwrapLastName :: LastName -> String
unwrapLastName (LastName str) = str

createQuestion :: String -> Either ErrorMessage Question
createQuestion = 
    createString "Question : " Question 1000

unwrapQuestion :: Question -> String
unwrapQuestion (Question str) = str

createAnswer :: String -> Either ErrorMessage Answer
createAnswer = 
    createString "Answer : " Answer 1000

unwrapAnswer :: Answer -> String
unwrapAnswer (Answer str) = str



--- TODO NEED LOTS OF IMPROVEMENTS
--- TODO NEED LOTS OF IMPROVEMENTS
--- TODO NEED LOTS OF IMPROVEMENTS
--- TODO NEED LOTS OF IMPROVEMENTS
creatDateTimeSpan :: String -> String -> String -> Either ErrorMessage DateTimeSpan
creatDateTimeSpan strdtStart strdtEnd separator =
    let                 
        dtstart = fmap (read) $ splitOn separator strdtStart  
        dtend = fmap (read) $ splitOn separator strdtEnd  
    
    in  if ((length dtstart == 6) && (length dtend == 6))
        then  
            let  
                d1 = DateTime (dtstart!!0) (dtstart!!1) (dtstart!!2) (dtstart!!3) (dtstart!!4) (dtstart!!5)
                d2 = DateTime (dtend!!0) (dtend!!1) (dtend!!2) (dtend!!3) (dtend!!4) (dtend!!5)    
            in 
                Right $ DateTimeSpan (d1, d2)
        else Left "Invalid date format"
                
                