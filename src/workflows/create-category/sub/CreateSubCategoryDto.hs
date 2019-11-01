
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}



module CreateSubCategoryDto where


import Data.Aeson
import CommonSimpleTypes
import CommonDtos
import CommonCompoundTypes
import CreateSubCategoryPublicTypes


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
-- DTOs for Create Sub Category workflow
-- ==========================================================================================



-- ----------------------------------------------------------------------------
-- DTO for CreateAttributeRefForm
-- ----------------------------------------------------------------------------




data CreateSubCategoryForm = CreateSubCategoryForm {
        ccode :: String
    ,   description :: String
    ,   parentId :: String
    ,   parentCode :: String
    ,   enblmntStatuss :: String
    ,   subCategries ::   [String]
    } deriving (Generic, Show)

instance ToJSON CreateSubCategoryForm 

instance FromJSON CreateSubCategoryForm



-- Helper functions for converting from / to domain as well as to other states

toUnvalidatedSubCategory :: CreateSubCategoryForm -> UnvalidatedSubCategory
toUnvalidatedSubCategory  CreateSubCategoryForm{..} = 
    UnvalidatedSubCategory {
            ucatCd = ccode
        ,   udescpt = description
        ,   uparentIdCd = (parentId, parentCode)
        ,   uEnblmnt = enblmntStatuss
        ,   usubCatgrs = subCategries
        }


-------------------------------------------------------------------------------
-- DTO for SubCategoryCreated Event
-- ----------------------------------------------------------------------------



data SubCategoryCreatedDto = SubCategoryCreatedDto {
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


instance ToJSON SubCategoryCreatedDto

instance FromJSON SubCategoryCreatedDto



-- Helper functions for converting from / to domain as well as to other states
toDomain :: SubCategoryCreatedDto -> Either ErrorMessage Category
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
        ,   categoryRootStatus = rtStts
        ,   categoryEnablementStatus = enblmntStts
        ,   categoryDescription = descpt
        ,   categoryRelatedSubCategories = Data.Set.fromList subs
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

fromSubCategoryCreated :: SubCategoryCreated -> SubCategoryCreatedDto
fromSubCategoryCreated catgr = 
    SubCategoryCreatedDto {
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
          (rtSttusType, prtCatgrId, prtCatgrCode) = fromRootStatus . categoryRootStatus $ catgr
          (enblmntSttus, enblmntReason) = fromEnblmntStatus . categoryEnablementStatus $ catgr
          descpt = uwrpLgDescpt . categoryDescription $ catgr
          subCatgrs = fmap uwrpCatgrId . toList . categoryRelatedSubCategories $ catgr




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

data CreateSubCategoryEventDto = 
        SubCatCR SubCategoryCreatedDto
    |   SubCatsADD SubCategoriesAddedDto 
    deriving (Generic, Show)

instance ToJSON CreateSubCategoryEventDto

---

type CreateSubAttributeEventResponse = Map String CreateSubCategoryEventDto

instance ToJSONKey CreateSubAttributeEventResponse

---

type RespCrtSubCatWorkflow = [CreateSubAttributeEventResponse] 


-- Helper function

fromCrtSubCatgrEvtDomain :: CreateSubCategoryEvent -> CreateSubAttributeEventResponse
fromCrtSubCatgrEvtDomain evt = 
    case evt of
        SubCategoryCreated rootAttr ->
            let key = "createsubcategory"
                val = fromSubCategoryCreated rootAttr
            in  singleton key (SubCatCR val)
        SubCategoriesAdded subcatAdded ->
            let key = "subcategoriesadded"
                val = fromSubCategoriesAdded subcatAdded
            in singleton key (SubCatsADD val)
        