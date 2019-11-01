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


-- Event will be created if the Acknowledgment was successfully posted
data DeclarationAcknowledgmentSent = DeclarationAcknowledgmentSent {
        declaredLostItemId :: LostItemId
    ,   ownerContactInfo :: ContactInformation 
    } deriving (Eq, Ord, Show)


-- Events to send to search context / persit to the event store
type LostItemDeclared = DeclaredLostItem
type LocationAdded = Location
type AttributeAdded = Attribute


-- Declare / Register Lost Item state            
data DeclaredLostItem = DeclaredLostItem {
        lostItemId                  :: LostItemId
    ,   lostItemName                :: ItemName
    ,   lostItemCategoryId          :: CategoryId
    ,   lostItemDescription         :: LongDescription
    ,   lostItemLocations           :: Set Location
    ,   lostItemDateAndTimeSpan     :: DateTimeSpan
    ,   lostItemRegistrationTime    :: UTCTime
    ,   lostItemAttributes          :: Set Attribute
    ,   lostItemOwner               :: Person
    } deriving (Eq, Ord, Show)


-- The possible events resulting from the Declare/Register Lost Item workflow
data DeclareLostItemEvent =
      LostItemDeclared LostItemDeclared
    | LocationsAdded [Location]
    | AttributesAdded [Attribute]
    | SearchableItemDeclared LostItemDeclared 
    | AcknowledgmentSent DeclarationAcknowledgmentSent
    deriving (Eq, Ord, Show)



-- --------------------------------------------------------------------------
-- error outputs (in the common module)
-- --------------------------------------------------------------------------




-- --------------------------------------------------------------------------
-- the workflow itself
-- --------------------------------------------------------------------------

type DeclareLostItem = 
    UnvalidatedLostItem -> Either WorkflowError [DeclareLostItemEvent]

