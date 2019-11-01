
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
        rcode                    :: String
    ,   description             :: String
    ,   enablementStatus        :: String
    ,   relatedSubCategories    ::   [String]
    } deriving (Generic, Show)

instance ToJSON CreateRootCategoryForm 

instance FromJSON CreateRootCategoryForm



-- Helper functions for converting from / to domain as well as to other states

toUnvalidatedRootCategory :: CreateRootCategoryForm -> UnvalidatedRootCategory
toUnvalidatedRootCategory  CreateRootCategoryForm{..}= 
    UnvalidatedRootCategory {
            urootCategoryCode =             rcode                 
        ,   urootCategoryDescription =      description          
        ,   urootCategoryEnablement =       enablementStatus     
        ,   urootCatgrRelatedsubCatgrs =    relatedSubCategories 
        }

        


-------------------------------------------------------------------------------
-- DTO for RootCategoryCreated Event
-- ----------------------------------------------------------------------------



data RootCategoryCreatedDto = RootCategoryCreatedDto {
        rcatId           :: String
    ,   rcatCode         :: String

    ,   rrtStatus        :: String
    ,   rprtCatgrId      :: String
    ,   rprtCatgrCode    :: String

    ,   renblmntStatus   :: String
    ,   renblmntReason   :: String

    ,   rcatDesc         :: String
    ,   rsubCategrs      :: [String]
    } deriving (Generic, Show)


instance ToJSON RootCategoryCreatedDto

instance FromJSON RootCategoryCreatedDto



-- Helper functions for converting from / to domain as well as to other states
rootCatgrDtoToDomain :: RootCategoryCreatedDto -> Either ErrorMessage Category
rootCatgrDtoToDomain dto = do
    id <- crtCatgrId . rcatId $ dto
    code <- crtCatgrCd . rcatCode $ dto

    let rootStatusInfo = (rrtStatus dto, rprtCatgrId dto,  rprtCatgrCode dto)

    rtStts <- toRootStatus rootStatusInfo

    let enblmntStatusInfo = (renblmntStatus dto, renblmntReason dto) 

    enblmntStts <- toEnablementStatus enblmntStatusInfo

    descpt <- crtLgDescpt . rcatDesc $ dto
    subs <- traverse crtCatgrId . rsubCategrs $ dto
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


type RSubCategoriesAddedDto = [AddedSubCategoryDto]






-- Helper functions for converting from / to domain as well as to other states

fromRootCategoryCreated :: RootCategoryCreated -> RootCategoryCreatedDto
fromRootCategoryCreated catgr = 
    RootCategoryCreatedDto {
            rcatId = id        
        ,   rcatCode = code     
        ,   rrtStatus = rtSttusType     
        ,   rprtCatgrId = prtCatgrId   
        ,   rprtCatgrCode = prtCatgrCode
        ,   renblmntStatus = enblmntSttus
        ,   renblmntReason = enblmntReason  
        ,   rcatDesc = descpt        
        ,   rsubCategrs = subCatgrs     
    }
    where id = uwrpCatgrId . categoryId $ catgr
          code = uwpCatgrCd . categoryCode $ catgr
          (rtSttusType, prtCatgrId, prtCatgrCode) = fromRootStatus . categoryRootStatus $ catgr
          (enblmntSttus, enblmntReason) = fromEnblmntStatus . categoryEnablementStatus $ catgr
          descpt = uwrpLgDescpt . categoryDescription $ catgr
          subCatgrs = fmap uwrpCatgrId . toList . categoryRelatedSubCategories $ catgr




fromRootSubCategoriesAdded :: SubCategoriesAdded -> RSubCategoriesAddedDto
fromRootSubCategoriesAdded = 
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

data CreateRootCategoryEventDto = 
        RootCatCR RootCategoryCreatedDto
    |   RSubCatsADD RSubCategoriesAddedDto 
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
        RSubCategoriesAdded subcatAdded ->
            let key = "subcategoriesadded"
                val = fromRootSubCategoriesAdded subcatAdded
            in singleton key (RSubCatsADD val)
        