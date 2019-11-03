
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
subCatgrDtoToDomain :: SubCategoryCreatedDto -> Either ErrorMessage Category
subCatgrDtoToDomain dto = do
    id <- crtCatgrId . catId $ dto
    code <- crtCatgrCd . catCode $ dto
    enblmntStts <- toEnablementStatus (enblmntStatus dto, enblmntReason dto) 
    descpt <- crtLgDescpt . catDesc $ dto
    subs <- traverse crtCatgrId . subCategrs $ dto

    case rtStatus dto of
        "Root" -> 
            return $ RootCategory catgrInfo
                where   catgrInfo = CategoryInfo {
                            categoryId = id
                        ,   categoryCode = code
                        ,   categoryEnablementStatus = enblmntStts
                        ,   categoryDescription = descpt
                        ,   categoryRelatedSubCategories = Data.Set.fromList subs
                        }   
                    
        "Sub" -> 
            let (strPrntId, strPrntCd) = (prtCatgrId dto, prtCatgrCode dto)
            in  case (strPrntId, strPrntCd) of

                    ("","") -> 
                        return $ SubCategory catgrInfo Nothing
                            where   catgrInfo = CategoryInfo {
                                        categoryId = id
                                    ,   categoryCode = code
                                    ,   categoryEnablementStatus = enblmntStts
                                    ,   categoryDescription = descpt
                                    ,   categoryRelatedSubCategories = Data.Set.fromList subs
                                    } 
                                
                    _ ->
                        do  prntId <- crtCatgrId strPrntId
                            prntCd <- crtCatgrCd strPrntCd
                            let parentInfo = ParentInfo prntId prntCd

                            return $ SubCategory catgrInfo (Just parentInfo) 
                            where   catgrInfo = CategoryInfo {
                                        categoryId = id
                                    ,   categoryCode = code
                                    ,   categoryEnablementStatus = enblmntStts
                                    ,   categoryDescription = descpt
                                    ,   categoryRelatedSubCategories = Data.Set.fromList subs
                                    } 










-------------------------------------------------------------------------------
-- DTO for SubCategory Added Event
-- ----------------------------------------------------------------------------


type SSubCategoriesAddedDto = [AddedSubCategoryDto]





-- Helper functions for converting from / to domain as well as to other states


-- Helper functions for converting from / to domain as well as to other states

fromSubCategoryCreated :: SubCategoryCreated -> SubCategoryCreatedDto
fromSubCategoryCreated (RootCategory catgrInfo) = 
    SubCategoryCreatedDto {
        catId = toId catgrInfo     
    ,   catCode = toCode catgrInfo   
    ,   rtStatus = "Root"     
    ,   prtCatgrId = ""   
    ,   prtCatgrCode = ""
    ,   enblmntStatus = fst . toEnblmntStatus $ catgrInfo
    ,   enblmntReason = snd . toEnblmntStatus $ catgrInfo 
    ,   catDesc = toDescpt catgrInfo      
    ,   subCategrs = toSubCatgrs catgrInfo    
    }
fromSubCategoryCreated (SubCategory catgrInfo Nothing) = 
    SubCategoryCreatedDto {
        catId = toId catgrInfo    
    ,   catCode = toCode catgrInfo    
    ,   rtStatus = "Sub"     
    ,   prtCatgrId = ""   
    ,   prtCatgrCode = ""
    ,   enblmntStatus = fst . toEnblmntStatus $ catgrInfo
    ,   enblmntReason = snd . toEnblmntStatus $ catgrInfo
    ,   catDesc = toDescpt catgrInfo   
    ,   subCategrs = toSubCatgrs catgrInfo     
    }
fromSubCategoryCreated (SubCategory catgrInfo (Just (ParentInfo id cd))) = 
    SubCategoryCreatedDto {
        catId = toId catgrInfo      
    ,   catCode = toCode catgrInfo     
    ,   rtStatus = "Sub"     
    ,   prtCatgrId = uwrpCatgrId id   
    ,   prtCatgrCode = uwpCatgrCd cd
    ,   enblmntStatus = fst . toEnblmntStatus $ catgrInfo
    ,   enblmntReason = snd . toEnblmntStatus $ catgrInfo  
    ,   catDesc = toDescpt catgrInfo       
    ,   subCategrs = toSubCatgrs catgrInfo     
    }
 


-----

fromSubCategoryCreated :: SubCategoryCreated -> SubCategoryCreatedDto
fromSubCategoryCreated catgr = 
    SubCategoryCreatedDto {
            catId = toId catgr     
        ,   catCode = toCode catgr    
        ,   rtStatus = rtSttusType     
        ,   prtCatgrId = prtCatgrId   
        ,   prtCatgrCode = prtCatgrCode
        ,   enblmntStatus = enblmntSttus
        ,   enblmntReason = enblmntReason  
        ,   catDesc = descpt        
        ,   subCategrs = subCatgrs     
    }
    where toId = uwrpCatgrId . categoryId $ 
          toCode = uwpCatgrCd . categoryCode $ catgr
          (rtSttusType, prtCatgrId, prtCatgrCode) = fromRootStatus . categoryRootStatus $ catgr
          (enblmntSttus, enblmntReason) = fromEnblmntStatus . categoryEnablementStatus $ catgr
          descpt = uwrpLgDescpt . categoryDescription $ catgr
          subCatgrs = fmap uwrpCatgrId . toList . categoryRelatedSubCategories $ catgr




fromSubSubCategoriesAdded :: SubCategoriesAdded -> SSubCategoriesAddedDto
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
    |   SSubCatsADD SSubCategoriesAddedDto 
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
        