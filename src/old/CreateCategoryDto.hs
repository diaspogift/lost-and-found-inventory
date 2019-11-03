
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
rootCatgrDtoToDomain :: RootCategoryCreatedDto -> Either ErrorMessage Category
rootCatgrDtoToDomain dto = do
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


type RSubCategoriesAddedDto = [AddedSubCategoryDto]






-- Helper functions for converting from / to domain as well as to other states

fromRootCategoryCreated :: RootCategoryCreated -> RootCategoryCreatedDto
fromRootCategoryCreated (RootCategory catgrInfo) = 
    RootCategoryCreatedDto {
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
fromRootCategoryCreated (SubCategory catgrInfo parentInfo) = 
    case parentInfo of 
        Nothing ->
            RootCategoryCreatedDto {
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
        Just (ParentInfo id cd) ->

            RootCategoryCreatedDto {
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
 

-- helpers

toId = uwrpCatgrId . categoryId 
toCode = uwpCatgrCd . categoryCode 
--(rtSttusType, prtCatgrId, prtCatgrCode) = fromRootStatus . categoryRootStatus $ catgr
toEnblmntStatus = fromEnblmntStatus . categoryEnablementStatus 
toDescpt = uwrpLgDescpt . categoryDescription 
toSubCatgrs = fmap uwrpCatgrId . toList . categoryRelatedSubCategories



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
        