module DeclaredLostItemPublicTypes where

import CommonSimpleTypes
import CommonCompoundTypes

import Control.Exception.Base
import Data.Set
import Data.Time
import Data.Dates




-- ==========================================================================
-- This file contains the definitions of PUBLIC types 
-- (exposed at the boundary of the bounded context )
-- related to the Declare LostItem workflow 
-- ==========================================================================

-- --------------------------------------------------------------------------
-- inputs to the workflow


data UnvalidatedLocation = UnvalidatedLocation {
        uregion :: String
    ,   udivision :: String
    ,   usubdivision :: String
    ,   ucity :: String
    ,   uvillage :: String
    ,   uneighborhood :: String
    ,   uloaddress :: String
    } deriving (Eq, Ord, Show)

data UnvalidatedAttribute = UnvalidatedAttribute {
        uattrCode :: String
      , uattrName :: String
      , uattrDescription :: String
      , uattrValue :: String
      , uattrUnit ::   String
      , urelatedCategories :: [(String, String)]
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


-- Event will be created if the Acknowledgment was successfully posted
data DeclarationAcknowledgmentSent = DeclarationAcknowledgmentSent {
        declaredLostItemId :: LostItemId
    ,   ownerContactInfo :: ContactInformation 
    }

-- Event to send to search context
type LostItemDeclared = DeclaredLostItem


-- Declared state            


data DeclaredLostItem = DeclaredLostItem {
        lostItemId                  :: LostItemId
    ,   lostItemName                :: ItemName
    ,   lostItemCategoryId          :: CategoryId
    ,   lostItemLocation            :: Set Location
    ,   lostItemDesc                :: LongDescription
    ,   lostItemRegistrationTime    :: UTCTime
    ,   lostItemDateAndTimeSpan     :: DateTimeSpan
    ,   lostItemAttributes          :: Set Attribute
    ,   lostItemOwner               :: Person
    } deriving (Eq, Ord, Show)



-- The possible events resulting from the PlaceOrder workflow
-- Not all events will occur, depending on the logic of the workflow
data DeclareLostItemEvent =
      LostItemDeclared LostItemDeclared
    | SearchableItemDeclared LostItemDeclared 
    | AcknowledgmentSent DeclarationAcknowledgmentSent



-- --------------------------------------------------------------------------
-- error outputs 


-- All the things that can go wrong in this workflow
newtype ValidationError = ValidationError String


newtype DbError = DbError String


data ServiceInfo = ServiceInfo {
        serviceName :: String
    ,   endpoint :: String
    }

data RemoteServiceError = RemoteServiceError {
        service :: ServiceInfo
    ,   execption :: IOException
    }

data DeclareLostItemError =
      Validation ValidationError 
    | Remote RemoteServiceError
    | Db DbError 


-- --------------------------------------------------------------------------
-- // the workflow itself

type DeclareLostItem = 
    UnvalidatedLostItem -> Either DeclareLostItemError [DeclareLostItemEvent]

