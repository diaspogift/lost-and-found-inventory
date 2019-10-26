module CommonSimpleTypes (
      LostItemId, FoundItemId, DateTimeSpan
    , MatchedItemId, ClaimedItemId
    , UserId, TenantId
    , RegionName, DivisionName, SubDivisionName
    , CategoryId, CategoryCode, ParentCategoryId, ShortDescription, LongDescription
    , TenantName, ItemName, City, Village, Neighborhood, Address
    , AttributeCode, AttributeName, AttributeValue, AttributeUnit
    , EmailAddress, PostalAddress, Telephone
    , FirstName, Middle, LastName
    , Question, Answer, ErrorMessage, Reason, CityOrVillage (..) -- , Urban, Country
    , ValidationError (..), DbError (..), RemoteServiceError (..), ServiceInfo (..), WorkflowError (..)
    , crtFndItmId, crtLstItmId
    , crtMtchdItmId, crtClmdItmId
    , crtUsrId, crtTntId, crtCatgrId, crtCatgrCd
    , crtShrtDescpt, crtLgDescpt
    , crtTntNm, crtItmNm, crtCity, crtVillage, crtNghbrhd, crtAddress
    , crtAttrCd, crtAttrNm, crtOptAttrUnt, crtOptAttrVal, crtAttrUnt, crtAttrVal
    , crtEmailAddress, crtPstAddress, crtTel, crtOptTel, crtOptNghbrhd
    , crtFstNm, crtMdleNm, crtLstNm
    , crtQuest, crtAns, crtDtTmSpan , crtOptPstAddrss
    
    , uwrpFndItmId, uwrpLstItmId, uwrpDtTmSpan
    , uwrpMtchdItmId, uwrpClmdItmId
    , uwrpUsrId, uwrpTntId, uwrpCatgrId, uwpCatgrCd
    , uwrpShrtDescpt, uwrpLgDescpt
    , uwrpTntNm, uwrpItmNm, uwrpCity, uwrpVillage, uwrpNghbrhd, uwrpAddress
    , uwrpAttrCd, uwrpAttrNm, uwrpOptAttrVal, uwrpOptAttrUnt, uwrpAttrVal, uwrpAttrUnt
    , uwrpEmailAddress, uwrpPostalAddress, uwrpTel
    , uwrpFstNm, uwrpMdleNm, uwrpLstNm
    , uwrpQuest, uwrpAns,  uwrpOptPstAddress
    ) where 


import Data.Monoid
import qualified Text.Email.Validate as EmailVal
import qualified Data.ByteString.Char8 as Char8
import Data.Dates
import Data.List.Split
import Text.Read
import Data.Maybe







-- ===================================================================================
-- Simple types and constrained types related to the Lost |&| Found Inventory domain.
--
-- E.g. Single case discriminated unions (aka wrappers), enums, etc
-- ===================================================================================



type Reason = String
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

data CityOrVillage =
      Urban City 
    | Country Village
    deriving (Eq, Ord, Show)


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



-- =============================================================================
-- Common shared error types
-- =============================================================================



type ErrorMessage 
    = String

-- All the things that can go wrong 
newtype ValidationError = 
    ValidationError String deriving (Eq, Ord, Show)

-- Database related errors
newtype DbError = 
    DbError String deriving (Eq, Ord, Show)

-- External systems errors
data ServiceInfo = ServiceInfo {
        serviceName :: String
    ,   endpoint :: String
    } deriving (Eq, Ord, Show)

data RemoteServiceError = RemoteServiceError {
        service :: ServiceInfo
    ,   execption :: String -- SomeException
    ,   errorCode :: Int
    } deriving (Eq, Ord, Show)

-- shared workflow error
data WorkflowError =
      Validation ValidationError 
    | Remote RemoteServiceError
    | Db DbError 
    deriving (Eq, Ord, Show)




-- ===================================================================================
-- Reusable constructors and getters for constrained types
-- Reusable uwrppers to primitives types such as String, Int ..etc
-- ===================================================================================





crtString :: String 
                -> (String -> a) 
                -> Int 
                -> String 
                -> Either ErrorMessage a
crtString fieldName ctor maxLen str  
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


crtBoundedString :: String 
                -> (String -> a) 
                -> Int 
                -> Int
                -> String 
                -> Either ErrorMessage a
crtBoundedString fieldName ctor minLen maxLen str       
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

             
crtStringOpt :: String 
                -> (String -> a) 
                -> Int 
                -> String 
                -> Either ErrorMessage (Maybe a)
crtStringOpt fieldName ctor maxLen str
    | null str =
        Right Nothing
    | length str > maxLen =
        let errorMsg = fieldName <> " must not be more than " <> show maxLen <> " chars"   
        in Left errorMsg 
    | otherwise =
        Right $ Just $ ctor str 


crtNum :: (Num a, Eq a, Ord a, Show a) =>
               String 
            -> (a -> a) 
            -> a 
            -> a
            -> a 
            -> Either ErrorMessage a
crtNum fieldName ctor minVal maxVal i
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


crtEmail :: String 
            -> (String -> EmailAddress) 
            -> String 
            -> Either ErrorMessage EmailAddress
crtEmail fieldName  ctor  str 
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


crtLstItmId :: String -> Either ErrorMessage LostItemId
crtLstItmId = 
    crtBoundedString "Lost Item Identifier: " LostItemId 36 36 

uwrpLstItmId :: LostItemId -> String
uwrpLstItmId (LostItemId str) = str

crtFndItmId :: String -> Either ErrorMessage FoundItemId
crtFndItmId = 
    crtBoundedString "Found Item Identifier: " FoundItemId 36 36

uwrpFndItmId :: FoundItemId -> String
uwrpFndItmId (FoundItemId str) = str

crtMtchdItmId :: String -> Either ErrorMessage MatchedItemId
crtMtchdItmId = 
    crtBoundedString "Matched Item Identifier: " MatchedItemId 36 36

uwrpMtchdItmId :: MatchedItemId -> String
uwrpMtchdItmId (MatchedItemId str) = str

crtClmdItmId :: String -> Either ErrorMessage ClaimedItemId
crtClmdItmId = 
    crtBoundedString "Claimed Item Identifier: " ClaimedItemId 36 36

uwrpClmdItmId :: ClaimedItemId -> String
uwrpClmdItmId (ClaimedItemId str) = str

crtUsrId :: String -> Either ErrorMessage UserId
crtUsrId = 
    crtBoundedString "User Identifier: " UserId 36 36

uwrpUsrId :: UserId -> String
uwrpUsrId (UserId str) = str

crtTntId :: String -> Either ErrorMessage TenantId
crtTntId = 
    crtBoundedString "Tenant Identifier: " TenantId 36 36

uwrpTntId :: TenantId -> String
uwrpTntId (TenantId str) = str


crtCatgrId :: String -> Either ErrorMessage CategoryId
crtCatgrId = 
    crtBoundedString "Category Identifier: " CategoryId 36 36

uwrpCatgrId :: CategoryId -> String
uwrpCatgrId (CategoryId str) = str

crtCatgrCd :: String -> Either ErrorMessage CategoryCode
crtCatgrCd = 
    crtBoundedString "Category Code: " CategoryCode 10 20

uwpCatgrCd :: CategoryCode -> String
uwpCatgrCd (CategoryCode str) = str

crtShrtDescpt :: String -> Either ErrorMessage ShortDescription
crtShrtDescpt = 
    crtString "Short Description: " ShortDescription 250

uwrpShrtDescpt:: ShortDescription -> String
uwrpShrtDescpt (ShortDescription str) = str

crtLgDescpt :: String -> Either ErrorMessage LongDescription
crtLgDescpt = 
    crtString "Long Description: " LongDescription 5000

uwrpLgDescpt:: LongDescription -> String
uwrpLgDescpt (LongDescription str) = str
    
crtTntNm :: String -> Either ErrorMessage TenantName
crtTntNm = 
    crtString "Tenant Name: " TenantName 100

uwrpTntNm :: TenantName -> String
uwrpTntNm (TenantName str) = str

crtItmNm :: String -> Either ErrorMessage ItemName
crtItmNm = 
    crtString "Item Name: " ItemName 500

uwrpItmNm :: ItemName -> String
uwrpItmNm (ItemName str) = str

crtCity :: String -> Either ErrorMessage City
crtCity = 
    crtString "City: " City 100

uwrpCity:: City -> String
uwrpCity (City str) = str

crtVillage :: String -> Either ErrorMessage Village
crtVillage = 
    crtString "Village: " Village 100

uwrpVillage :: Village -> String
uwrpVillage (Village str) = str

crtNghbrhd :: String -> Either ErrorMessage (Maybe Neighborhood)
crtNghbrhd = 
    crtStringOpt "Neighborhood: " Neighborhood 500

crtOptNghbrhd :: String -> Either ErrorMessage (Maybe Neighborhood)
crtOptNghbrhd = 
    crtStringOpt "Neighborhood: " Neighborhood 500

uwrpNghbrhd :: Neighborhood -> String
uwrpNghbrhd (Neighborhood str) = str

crtAddress :: String -> Either ErrorMessage Address
crtAddress = 
    crtString "Address: " Address 500

uwrpAddress :: Address -> String
uwrpAddress (Address str) = str

crtAttrCd :: String -> Either ErrorMessage AttributeCode
crtAttrCd = 
    crtString "Attribute Code: " AttributeCode 50

uwrpAttrCd :: AttributeCode -> String
uwrpAttrCd (AttributeCode str) = str

crtAttrNm :: String -> Either ErrorMessage AttributeName
crtAttrNm = 
    crtString "Attribute Name: " AttributeName 50

uwrpAttrNm :: AttributeName -> String
uwrpAttrNm (AttributeName str) = str

crtOptAttrVal :: String -> Either ErrorMessage (Maybe AttributeValue)
crtOptAttrVal = 
    crtStringOpt "Attribute Value: " AttributeValue 50

uwrpOptAttrVal :: Maybe AttributeValue -> String
uwrpOptAttrVal (Just (AttributeValue str)) = str
uwrpOptAttrVal Nothing = ""


crtAttrVal :: String -> Either ErrorMessage AttributeValue
crtAttrVal = 
    crtString "Attribute Value: " AttributeValue 50


uwrpAttrVal :: AttributeValue -> String
uwrpAttrVal (AttributeValue str) = str


crtOptAttrUnt :: String -> Either ErrorMessage (Maybe AttributeUnit)
crtOptAttrUnt = 
    crtStringOpt "Attribute Unit: " AttributeUnit 50

uwrpOptAttrUnt :: Maybe AttributeUnit -> String
uwrpOptAttrUnt (Just (AttributeUnit str)) = str
uwrpOptAttrUnt Nothing = ""

crtAttrUnt :: String -> Either ErrorMessage AttributeUnit
crtAttrUnt = 
    crtString "Attribute Unit: " AttributeUnit 50

uwrpAttrUnt :: AttributeUnit -> String
uwrpAttrUnt (AttributeUnit str) = str

crtEmailAddress :: String -> Either ErrorMessage EmailAddress
crtEmailAddress = 
    crtEmail "Email Address : " EmailAddress

uwrpEmailAddress :: EmailAddress -> String
uwrpEmailAddress (EmailAddress str) = str

crtPstAddress :: String -> Either ErrorMessage PostalAddress
crtPstAddress = 
    crtString "Postal Address: " PostalAddress 500

uwrpPostalAddress :: PostalAddress -> String
uwrpPostalAddress (PostalAddress str) = str


crtOptPstAddrss :: String -> Either ErrorMessage (Maybe PostalAddress)
crtOptPstAddrss = 
    crtStringOpt "Postal Address: " PostalAddress 500

uwrpOptPstAddress :: Maybe PostalAddress -> String
uwrpOptPstAddress (Just (PostalAddress str)) = str
uwrpOptPstAddress Nothing = ""

crtTel :: String -> Either ErrorMessage Telephone
crtTel = 
    crtString "Telephone: " Telephone 50  
  
crtOptTel :: String -> Either ErrorMessage (Maybe Telephone)
crtOptTel = 
    crtStringOpt "Telephone: " Telephone 50 


uwrpTel :: Telephone -> String
uwrpTel (Telephone str) = str
    
crtFstNm :: String -> Either ErrorMessage FirstName
crtFstNm = 
    crtString "First Name : " FirstName 100

uwrpFstNm:: FirstName -> String
uwrpFstNm (FirstName str) = str

crtMdleNm :: String -> Either ErrorMessage (Maybe Middle)
crtMdleNm = 
    crtStringOpt "Middle Name : " Middle 100

uwrpMdleNm :: Maybe Middle -> String
uwrpMdleNm (Just (Middle str)) = str
uwrpMdleNm Nothing = ""

crtLstNm :: String -> Either ErrorMessage LastName
crtLstNm = 
    crtString "Last Name : " LastName 100

uwrpLstNm :: LastName -> String
uwrpLstNm (LastName str) = str

crtQuest :: String -> Either ErrorMessage Question
crtQuest = 
    crtString "Question : " Question 1000

uwrpQuest :: Question -> String
uwrpQuest (Question str) = str

crtAns :: String -> Either ErrorMessage Answer
crtAns = 
    crtString "Answer : " Answer 1000

uwrpAns :: Answer -> String
uwrpAns (Answer str) = str

--- TODO: MIGHT STILL NEED SOME IMPROVEMENTS

crtDtTmSpan :: String -> String -> String -> Either ErrorMessage DateTimeSpan
crtDtTmSpan strdtStart strdtEnd separator =
    let                 
        maybeDtstart = fmap (readMaybe :: String-> Maybe Int) $ splitOn separator strdtStart  
        maybeDtend = fmap (readMaybe :: String-> Maybe Int) $ splitOn separator strdtEnd  
        dtstart = catMaybes maybeDtstart
        dtend = catMaybes maybeDtend
        
    
    in  if ((length dtstart == 6) && (length dtend == 6))
        then 
            let  
                dsy = dtstart!!0
                dsmo = dtstart!!1
                dsd = dtstart!!2
                dsh = dtstart!!3
                dsmi = dtstart!!4
                dss = dtstart!!5

                dey = dtend!!0
                demo = dtend!!1
                ded = dtend!!2
                deh = dtend!!3
                demi = dtend!!4
                des = dtend!!5

                
                
            in  if (checkDateAttrRanges (dsy, dsmo, dsd, dsh, dsmi, dss) && checkDateAttrRanges (dey, demo, ded, deh, demi, des)) 
                then    
                        let d1 = DateTime dsy dsmo dsd dsh dsmi dss
                            d2 = DateTime dey demo ded deh demi des
                
                        in  if d1 > d2 
                            then Left "Invalid date time span order"
                            else Right $ DateTimeSpan (d1, d2)
                else Left "Invalid date time span range (1 < month 12) (1 < day 31) (00 < hour < 23) (00 < min < 59) (00 < sec < 59)"

        else Left "Invalid date format: y m d h m s are required and must all be numbers"
    where checkDateAttrRanges (y, mo, d, h, mi, s)
            | mo > 12 || mo < 1 = False
            | d > 31 || d < 1 = False
            | h > 23 || h < 0 = False
            | mi > 59 || mi < 0 = False
            | s > 59 || s < 0 = False
            | otherwise = True
                
uwrpDtTmSpan :: DateTimeSpan -> (String, String)
uwrpDtTmSpan (DateTimeSpan (start, end)) = 
    ( show start, show end)

-- Check Interval library

