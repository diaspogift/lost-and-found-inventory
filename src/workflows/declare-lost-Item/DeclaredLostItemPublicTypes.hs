module DeclaredLostItemPublicTypes where

import CommonSimpleTypes
import CommonCompoundTypes

import Control.Exception
import Data.Set
import Data.Time
import Data.Dates




-- ==========================================================================
-- This file contains the definitions of PUBLIC types 
-- (exposed at the boundary of the bounded context)
-- related to the DeclareLostItem workflow 
-- ==========================================================================

-- --------------------------------------------------------------------------
-- inputs to the workflow
-- --------------------------------------------------------------------------
type UnvalidatedLostItemId = String

type UnvalidatedAminitrativeArea = (String, String, String)


data UnvalidatedLocation = UnvalidatedLocation {
        uadminArea :: UnvalidatedAminitrativeArea
    ,   ucity :: String
    ,   uvillage :: String
    ,   uneighborhood :: String
    ,   uloaddresses :: [String]
    } deriving (Eq, Ord, Show)

data UnvalidatedAttribute = UnvalidatedAttribute {
      uattrCode :: String
    , uattrName :: String
    , uattrDescription :: String
    , uattrValue :: String
    , uattrUnit ::   String
    } deriving (Eq, Ord, Show)

data UnvalidatedPerson = UnvalidatedPerson {
        uuserId :: String
    ,   ucontact :: UnvalidatedContactInformation
    ,   ufullname :: UnvalidatedFullName
    } deriving (Eq, Ord, Show)

data UnvalidatedContactInformation = UnvalidatedContactInformation {
        uemail :: String
    ,   uaddress :: String
    ,   uprimaryTel :: String
    ,   usecondaryTel :: String 
    } deriving (Eq, Ord, Show)

data UnvalidatedFullName = UnvalidatedFullName {
        ufirst :: String
    ,   umiddle :: String
    ,   ulast :: String  
    } deriving (Eq, Ord, Show)

data UnvalidatedLostItem = UnvalidatedLostItem {
        uliName :: String
    ,   uliCategoryId :: String
    ,   uliDescription :: String
    ,   uliDateAndTimeSpan :: (String, String)
    ,   ulocations :: [UnvalidatedLocation]
    ,   uliattributes :: [UnvalidatedAttribute]
    ,   uowner :: UnvalidatedPerson   
    } deriving (Eq, Ord, Show)


-- --------------------------------------------------------------------------
-- outputs from the workflow (success case)
-- --------------------------------------------------------------------------


-- Event will be crtd if the Acknowledgment was successfully posted
data DeclarationAcknowledgmentSent = DeclarationAcknowledgmentSent {
        declaredLostItemId :: LostItemId
    ,   ownerContactInfo :: ContactInformation 
    } deriving (Eq, Ord, Show)

-- Event to send to search context
type LostItemDeclared = DeclaredLostItem


-- Declare / Register Lost Item state            
data DeclaredLostItem = DeclaredLostItem {
        lostItemId                  :: LostItemId
    ,   lostItemName                :: ItemName
    ,   lostItemCategoryId          :: CategoryId
    ,   lostItemDesc                :: LongDescription
    ,   lostItemLocation            :: Set Location
    ,   lostItemDateAndTimeSpan     :: DateTimeSpan
    ,   lostItemRegistrationTime    :: UTCTime
    ,   lostItemAttributes          :: Set Attribute
    ,   lostItemOwner               :: Person
    } deriving (Eq, Ord, Show)


-- The possible events resulting from the Declare/Register Lost Item workflow
data DeclareLostItemEvent =
      LostItemDeclared LostItemDeclared
    | SearchableItemDeclared LostItemDeclared 
    | AcknowledgmentSent DeclarationAcknowledgmentSent
    deriving (Eq, Ord, Show)



-- --------------------------------------------------------------------------
-- error outputs 
-- --------------------------------------------------------------------------


-- All the things that can go wrong in this workflow
newtype ValidationError = 
    ValidationError String deriving (Eq, Ord, Show)


newtype DbError = 
    DbError String deriving (Eq, Ord, Show)


data ServiceInfo = ServiceInfo {
        serviceName :: String
    ,   endpoint :: String
    } deriving (Eq, Ord, Show)

data RemoteServiceError = RemoteServiceError {
        service :: ServiceInfo
    ,   execption :: String -- SomeException
    ,   errorCode :: Int
    } deriving (Eq, Ord, Show)

data DeclareLostItemError =
      Validation ValidationError 
    | Remote RemoteServiceError
    | Db DbError 
    deriving (Eq, Ord, Show)


-- --------------------------------------------------------------------------
-- the workflow itself
-- --------------------------------------------------------------------------

type DeclareLostItem = 
    UnvalidatedLostItem -> Either DeclareLostItemError [DeclareLostItemEvent]

