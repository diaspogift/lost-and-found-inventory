module RegisterLostItemWorkflow where

import DomainCommonTypes
import DomainTypes


-- Public types

data LostItemDto = LostItemDto {
        uliIdentifier :: String
    ,   uliName :: String
    ,   uliCategoryId :: String
    ,   ulostLocation :: LocationDto
    ,   uliDescription :: String
    ,   uliAttributes :: [AttributeDto]
    ,   uliOwner :: PersonDto
    } deriving (Eq, Ord, Show)

data LocationDto = LocationDto {
        uregion :: String
    ,   udivision :: String
    ,   usubdivision :: String
    ,   ucity :: String
    ,   uvillage :: String
    ,   uneighborhood :: String
    ,   uloAddress :: String
    } deriving (Eq, Ord, Show)

data AttributeDto = AttributeDto {
        uattrCode :: String
      , uattrName :: String
      , uattrDescription :: String
      , uattrValue :: String
      , uattrUnit ::   String
      , urelatedCategory :: String
      , urelatedCategoryType :: String
    } deriving (Eq, Ord, Show)

data PersonDto = PersonDto {
      uuser :: String
    , ucontact :: ContactInformationDto
    , uname :: FullNameDto
    } deriving (Eq, Ord, Show)

data ContactInformationDto = ContactInformationDto {
    -- Tel required, email optional
      uemail :: String
    , uaddress :: String
    , uprimaryTel :: String
    , usecondaryTel :: String
    } deriving (Eq, Ord, Show)
  
data FullNameDto = FullNameDto {

      ufirst :: String
    , umiddle :: String
    , ulast :: String
    } deriving (Eq, Ord, Show)




{-- Defining the comand type
data Command c = Command {
      data :: c
    , timStamp :: UTCTime
    , userId :: String
    }--}




--data RegisterLostItemCommand = Command UnvalidatedGroup
        


-- Ouputs of the provisioned group worflow 
    -- Sucess types




 


--  Failure types

data RegisterLostItemError = 
      ValidationError  String
    | DbError String


--  Worflow type 

type RegisterLostItemWorkflow = 
    LostItemDto -> Either RegisterLostItemError LostItemDto
