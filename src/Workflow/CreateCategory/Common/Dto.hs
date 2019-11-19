{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Workflow.CreateCategory.Common.Dto where

import Common.CompoundTypes
    (AttributeRef (..),
    Category (..),
    CategoryInfo (..),
    EnablementStatus (..),
    ParentInfo (..),
    AddedSubCategory (..),
    toEnablementStatus,
    fromEnblmntStatus
    )
import Common.Dtos
import Common.SimpleTypes
import Workflow.CreateCategory.Common.PublicTypes
import Workflow.CreateCategory.Root.PublicTypes
import Workflow.CreateCategory.Sub.PublicTypes
import Data.Aeson
    (ToJSON, FromJSON, ToJSONKey)
import Data.Map 
    (Map, 
    singleton
    )
import Data.Set 
    (toList, 
    fromList
    )
import GHC.Generics 
    (Generic)




-- ==========================================================================================
-- This file contains the the logic for working with data transfer objects (DTOs)
-- Each type of DTO is defined using primitive, serializable types
-- ==========================================================================================




-- ==========================================================================================
-- DTOs for Create Root Category workflow
-- ==========================================================================================




-- ----------------------------------------------------------------------------
-- DTO for CreateAttributeRefForm
-- ----------------------------------------------------------------------------




-------------------------------------------------------------------------------
-- DTO for CategoryCreated Event
-- ----------------------------------------------------------------------------




data CategoryCreatedDto
  = CategoryCreatedDto
      { catId :: String,
        catCode :: String,
        rtStatus :: String,
        prtCatgrId :: String,
        prtCatgrCode :: String,
        enblmntStatus :: String,
        enblmntReason :: String,
        catDesc :: String,
        subCategrs :: [String]
      }
  deriving (Generic, Show)

instance ToJSON CategoryCreatedDto
instance FromJSON CategoryCreatedDto




--- Helper functions for converting from / to domain as well as to other states
---
---



catgrDtoToDomain :: CategoryCreatedDto -> Either ErrorMessage Category
catgrDtoToDomain dto = do
  id <- crtCatgrId . catId $ dto
  code <- crtCatgrCd . catCode $ dto
  enblmntStts <- toEnablementStatus (enblmntStatus dto, enblmntReason dto)
  descpt <- crtLgDescpt . catDesc $ dto
  subs <- traverse crtCatgrId . subCategrs $ dto
  case rtStatus dto of
    "Root" -> return $ RootCategory catgrInfo
      where catgrInfo = CategoryInfo
                { categoryId = id,
                  categoryCode = code,
                  categoryEnablementStatus = enblmntStts,
                  categoryDescription = descpt,
                  categoryRelatedSubCategories = Data.Set.fromList subs
                }
    "Sub" -> let (strPrntId, strPrntCd) = (prtCatgrId dto, prtCatgrCode dto)
       in case (strPrntId, strPrntCd) of
            ("", "") -> return $ SubCategory catgrInfo Nothing
              where catgrInfo = CategoryInfo
                        { categoryId = id,
                          categoryCode = code,
                          categoryEnablementStatus = enblmntStts,
                          categoryDescription = descpt,
                          categoryRelatedSubCategories = Data.Set.fromList subs
                        }
            _ -> do prntId <- crtCatgrId strPrntId
                    prntCd <- crtCatgrCd strPrntCd
                    let parentInfo = ParentInfo prntId prntCd
                    return $ SubCategory catgrInfo (Just parentInfo)
                 where catgrInfo = CategoryInfo
                        { categoryId = id,
                          categoryCode = code,
                          categoryEnablementStatus = enblmntStts,
                          categoryDescription = descpt,
                          categoryRelatedSubCategories = Data.Set.fromList subs
                        }




-------------------------------------------------------------------------------
-- DTO for SubCategory Added Event
-- ----------------------------------------------------------------------------




type SubCategoriesAddedDto = [AddedSubCategoryDto]




--- Helper functions for converting from / to domain as well as to other states
---
---




fromCategoryCreated :: CategoryCreated -> CategoryCreatedDto
fromCategoryCreated (RootCategory catgrInfo) =
  CategoryCreatedDto
    { catId = toId catgrInfo,
      catCode = toCode catgrInfo,
      rtStatus = "Root",
      prtCatgrId = "",
      prtCatgrCode = "",
      enblmntStatus = fst . toEnblmntStatus $ catgrInfo,
      enblmntReason = snd . toEnblmntStatus $ catgrInfo,
      catDesc = toDescpt catgrInfo,
      subCategrs = toSubCatgrs catgrInfo
    }
fromCategoryCreated (SubCategory catgrInfo Nothing) =
  CategoryCreatedDto
    { catId = toId catgrInfo,
      catCode = toCode catgrInfo,
      rtStatus = "Sub",
      prtCatgrId = "",
      prtCatgrCode = "",
      enblmntStatus = fst . toEnblmntStatus $ catgrInfo,
      enblmntReason = snd . toEnblmntStatus $ catgrInfo,
      catDesc = toDescpt catgrInfo,
      subCategrs = toSubCatgrs catgrInfo
    }
fromCategoryCreated (SubCategory catgrInfo (Just (ParentInfo id cd))) =
  CategoryCreatedDto
    { catId = toId catgrInfo,
      catCode = toCode catgrInfo,
      rtStatus = "Sub",
      prtCatgrId = uwrpCatgrId id,
      prtCatgrCode = uwpCatgrCd cd,
      enblmntStatus = fst . toEnblmntStatus $ catgrInfo,
      enblmntReason = snd . toEnblmntStatus $ catgrInfo,
      catDesc = toDescpt catgrInfo,
      subCategrs = toSubCatgrs catgrInfo
    }




--- helper funtions
---
---




toId = uwrpCatgrId . categoryId

toCode = uwpCatgrCd . categoryCode

toEnblmntStatus = fromEnblmntStatus . categoryEnablementStatus

toDescpt = uwrpLgDescpt . categoryDescription

toSubCatgrs = fmap uwrpCatgrId . toList . categoryRelatedSubCategories




fromSubCategoriesAdded :: SubCategoriesAdded -> SubCategoriesAddedDto
fromSubCategoriesAdded = fmap toSubCategoriesAddedDto
  where toSubCategoriesAddedDto AddedSubCategory {..} =
          AddedSubCategoryDto
            { parent = uwrpCatgrId addedSubCategoryParent,
              sub = uwrpCatgrId addSubCategoryId
            }




-- ----------------------------------------------------------------------------
-- DTO for WorkflowError
-- ----------------------------------------------------------------------------




data CreateCategoryEventDto
  = CatgrCreated CategoryCreatedDto
  | SubCatgrsAdded SubCategoriesAddedDto
  deriving (Generic, Show)

instance ToJSON CreateCategoryEventDto




type CreateCategoryEventResponse = Map String CreateCategoryEventDto

instance ToJSONKey CreateCategoryEventResponse




type RespCrtCatgrWorkflow = [CreateCategoryEventResponse]



--- Helper function
---
---



fromCrtCatgrEvtDomain :: CreateCategoryEvent -> CreateCategoryEventResponse
fromCrtCatgrEvtDomain (CategoryCreated catgr) =
  let key = "createrootcategory"
      val = fromCategoryCreated catgr in
   singleton key (CatgrCreated val)
fromCrtCatgrEvtDomain (SubCategoriesAdded subcatgrs) =
  let key = "subcategoriesadded"
      val = fromSubCategoriesAdded subcatgrs in
  singleton key (SubCatgrsAdded val)
