
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}



module CreateRootCategoryDto where


import Data.Aeson
import CommonSimpleTypes
import CommonDtos
import CommonCompoundTypes
import CreateRootCategoryPublicTypes


import Prelude hiding (last, id)
import Data.Time
import Data.Set hiding (null, singleton)
import Data.Map hiding (null, toList)

import GHC.Generics







-- ==========================================================================================
-- This file contains the the logic for working with data transfer objects (DTOs)
--
-- Each type of DTO is defined using primitive, serializable types
--
-- ==========================================================================================


-- ==========================================================================================
-- DTOs for Create Root Category workflow
-- ==========================================================================================



-- ----------------------------------------------------------------------------
-- DTO for CreateAttributeRefForm
-- ----------------------------------------------------------------------------




data CreateRootCategoryForm = CreateRootCategoryForm {
        ccode :: String
    ,   description :: String
    ,   enblmntStatuss :: String
    ,   subCategries ::   [String]
    } deriving (Generic, Show)

instance ToJSON CreateRootCategoryForm 

instance FromJSON CreateRootCategoryForm



-- Helper functions for converting from / to domain as well as to other states

toUnvalidatedRootCategory :: CreateRootCategoryForm -> UnvalidatedRootCategory
toUnvalidatedRootCategory  CreateRootCategoryForm{..}= 
    UnvalidatedRootCategory {
            ucatCd = ccode
        ,   udescpt = description
        ,   uEnblmnt = enblmntStatuss
        ,   usubCatgrs = subCategries
        }


-------------------------------------------------------------------------------
-- DTO for RootCategoryCreated Event
-- ----------------------------------------------------------------------------



data RootCategoryCreatedDto = RootCategoryCreatedDto {
        catId           :: String
    ,   catCode         :: String

    ,   rtStatus        :: String
    ,   prtCatgrId      :: String
    ,   prtCatgrCode    :: String

    ,   enblmntStatus   :: String
    ,   enblmntReason   :: String

    ,   catDesc         :: String
    ,   subCategrs      :: [String]
    } deriving (Generic, Show)


instance ToJSON RootCategoryCreatedDto

instance FromJSON RootCategoryCreatedDto



-- Helper functions for converting from / to domain as well as to other states
toDomain :: RootCategoryCreatedDto -> Either ErrorMessage Category
toDomain dto = do
    id <- crtCatgrId . catId $ dto
    code <- crtCatgrCd . catCode $ dto

    let rootStatusInfo = (rtStatus dto, prtCatgrId dto,  prtCatgrCode dto)

    rtStts <- toRootStatus rootStatusInfo

    let enblmntStatusInfo = (enblmntStatus dto, enblmntReason dto) 

    enblmntStts <- toEnablementStatus enblmntStatusInfo

    descpt <- crtLgDescpt . catDesc $ dto
    subs <- traverse crtCatgrId . subCategrs $ dto
    return 
        Category {
            categoryId = id
        ,   categoryCode = code
        ,   rootStatus = rtStts
        ,   enablementStatus = enblmntStts
        ,   categoryDesc = descpt
        ,   subCategories = Data.Set.fromList subs
        }










-------------------------------------------------------------------------------
-- DTO for SubCategory Added Event
-- ----------------------------------------------------------------------------


type SubCategoriesAddedDto = [AddedSubCategoryDto]

data AddedSubCategoryDto = AddedSubCategoryDto {
        parent  :: String 
    ,   sub     :: String
    } deriving (Generic, Show)

instance ToJSON AddedSubCategoryDto

instance FromJSON AddedSubCategoryDto



-- Helper functions for converting from / to domain as well as to other states

fromRootCategoryCreated :: RootCategoryCreated -> RootCategoryCreatedDto
fromRootCategoryCreated catgr = 
    RootCategoryCreatedDto {
            catId = id        
        ,   catCode = code     
        ,   rtStatus = rtSttusType     
        ,   prtCatgrId = prtCatgrId   
        ,   prtCatgrCode = prtCatgrCode
        ,   enblmntStatus = enblmntSttus
        ,   enblmntReason = enblmntReason  
        ,   catDesc = descpt        
        ,   subCategrs = subCatgrs     
    }
    where id = uwrpCatgrId . categoryId $ catgr
          code = uwpCatgrCd . categoryCode $ catgr
          (rtSttusType, prtCatgrId, prtCatgrCode) = fromRootStatus . rootStatus $ catgr
          (enblmntSttus, enblmntReason) = fromEnblmntStatus . enablementStatus $ catgr
          descpt = uwrpLgDescpt . categoryDesc $ catgr
          subCatgrs = fmap uwrpCatgrId . toList . subCategories $ catgr




fromSubCategoriesAdded :: SubCategoriesAdded -> SubCategoriesAddedDto
fromSubCategoriesAdded = 
    fmap toSubCategoriesAddedDto 
    where toSubCategoriesAddedDto AddedSubCategory{..} = 
            AddedSubCategoryDto {
                parent  = uwrpCatgrId parent 
            ,   sub = uwrpCatgrId sub
            } 

    

-- ----------------------------------------------------------------------------
-- DTO for WorkflowError 
-- ----------------------------------------------------------------------------

--- 

data CreateRootCategoryEventDto = 
        RootCatCR RootCategoryCreatedDto
    |   SubCatsADD SubCategoriesAddedDto 
    deriving (Generic, Show)

instance ToJSON CreateRootCategoryEventDto

---

type CreateRootAttributeEventResponse = Map String CreateRootCategoryEventDto

instance ToJSONKey CreateRootAttributeEventResponse

---

type RespCrtRootCatWorkflow = [CreateRootAttributeEventResponse] 


-- Helper function

fromCrtRootCatgrEvtDomain :: CreateRootCategoryEvent -> CreateRootAttributeEventResponse
fromCrtRootCatgrEvtDomain evt = 
    case evt of
        RootCategoryCreated rootAttr ->
            let key = "createrootcategory"
                val = fromRootCategoryCreated rootAttr
            in  singleton key (RootCatCR val)
        SubCategoriesAdded subcatAdded ->
            let key = "subcategoriesadded"
                val = fromSubCategoriesAdded subcatAdded
            in singleton key (SubCatsADD val)
        