
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
    ,   enblmntStatus :: String
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
        ,   uEnblmnt = enblmntStatus
        ,   usubCatgrs = subCategries
        }


-------------------------------------------------------------------------------
-- DTO for RootCategoryCreated Event
-- ----------------------------------------------------------------------------



data RootCategoryCreatedDto = RootCategoryCreatedDto {
        catId :: String
    ,   catCode :: String
    ,   rtStatus :: String
    ,   enblmntStatus :: String
    ,   catDesc :: String
    ,   subCategrs :: [String]
    } deriving (Generic, Show)


instance ToJSON RootCategoryCreatedDto

instance FromJSON RootCategoryCreatedDto



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
fromRootCategoryCreated = 
    RootCategoryCreatedDto
        <$> id <*> code <*> rtSttus <*> enblmntSttus  <*> descpt <*> subCatgrs
    where id = uwrpCatgrId . categoryId
          code = uwpCatgrCd . categoryCode
          rtSttus = fromRootStatus . rootStatus
          enblmntSttus = fromEnblmntStatus . enablementStatus
          descpt = uwrpLgDescpt . categoryDesc
          subCatgrs = fmap uwrpCatgrId . toList . subCategories

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
        