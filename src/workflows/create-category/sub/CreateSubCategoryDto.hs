
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
        scode                    :: String
    ,   description             :: String
    ,   parentId                :: String
    ,   parentCode              :: String
    ,   enblementStatus         :: String
    ,   relatedSubCategories    :: [String]
    } deriving (Generic, Show)

instance ToJSON CreateSubCategoryForm 

instance FromJSON CreateSubCategoryForm



-- Helper functions for converting from / to domain as well as to other states

toUnvalidatedSubCategory :: CreateSubCategoryForm -> UnvalidatedSubCategory
toUnvalidatedSubCategory  CreateSubCategoryForm{..} = 
    UnvalidatedSubCategory {
            usubCategoryCode          = scode
        ,   usubCategoryDescription   = description
        ,   usubCategoryParentIdandCd = (parentId, parentCode)
        ,   usubCatgrEnablementStatus = enblementStatus
        ,   usubCatgrRelatedsubCatgrs = relatedSubCategories
        }


-------------------------------------------------------------------------------
-- DTO for SubCategoryCreated Event
-- ----------------------------------------------------------------------------



data SubCategoryCreatedDto = SubCategoryCreatedDto {
        scatId           :: String
    ,   scatCode         :: String

    ,   srtStatus        :: String
    ,   sprtCatgrId      :: String
    ,   sprtCatgrCode    :: String

    ,   senblmntStatus   :: String
    ,   senblmntReason   :: String

    ,   scatDesc         :: String
    ,   ssubCategrs      :: [String]
    } deriving (Generic, Show)


instance ToJSON SubCategoryCreatedDto

instance FromJSON SubCategoryCreatedDto



-- Helper functions for converting from / to domain as well as to other states
subCatgrDtoToDomain :: SubCategoryCreatedDto -> Either ErrorMessage Category
subCatgrDtoToDomain dto = do
    id <- crtCatgrId . scatId $ dto
    code <- crtCatgrCd . scatCode $ dto

    let rootStatusInfo = (srtStatus dto, sprtCatgrId dto,  sprtCatgrCode dto)

    rtStts <- toRootStatus rootStatusInfo

    let enblmntStatusInfo = (senblmntStatus dto, senblmntReason dto) 

    enblmntStts <- toEnablementStatus enblmntStatusInfo

    descpt <- crtLgDescpt . scatDesc $ dto
    subs <- traverse crtCatgrId . ssubCategrs $ dto
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





-- Helper functions for converting from / to domain as well as to other states

fromSubCategoryCreated :: SubCategoryCreated -> SubCategoryCreatedDto
fromSubCategoryCreated catgr = 
    SubCategoryCreatedDto {
            scatId = id        
        ,   scatCode = code     
        ,   srtStatus = rtSttusType     
        ,   sprtCatgrId = prtCatgrId   
        ,   sprtCatgrCode = prtCatgrCode
        ,   senblmntStatus = enblmntSttus
        ,   senblmntReason = enblmntReason  
        ,   scatDesc = descpt        
        ,   ssubCategrs = subCatgrs     
    }
    where id = uwrpCatgrId . categoryId $ catgr
          code = uwpCatgrCd . categoryCode $ catgr
          (rtSttusType, prtCatgrId, prtCatgrCode) = fromRootStatus . categoryRootStatus $ catgr
          (enblmntSttus, enblmntReason) = fromEnblmntStatus . categoryEnablementStatus $ catgr
          descpt = uwrpLgDescpt . categoryDescription $ catgr
          subCatgrs = fmap uwrpCatgrId . toList . categoryRelatedSubCategories $ catgr




fromSubSubCategoriesAdded :: SubCategoriesAdded -> SubCategoriesAddedDto
fromSubSubCategoriesAdded = 
    fmap toSubCategoriesAddedDto 
    where toSubCategoriesAddedDto AddedSubCategory{..} = 
            AddedSubCategoryDto {
                parent  = uwrpCatgrId addedSubCategoryParent 
            ,   sub = uwrpCatgrId addSubCategoryId
            } 

    

-- ----------------------------------------------------------------------------
-- DTO for WorkflowError 
-- ----------------------------------------------------------------------------

--- 

data CreateSubCategoryEventDto = 
        SubCatCR SubCategoryCreatedDto
    |   SSubCatsADD SubCategoriesAddedDto 
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
        SSubCategoriesAdded subcatAdded ->
            let key = "subcategoriesadded"
                val = fromSubSubCategoriesAdded subcatAdded
            in singleton key (SSubCatsADD val)
        