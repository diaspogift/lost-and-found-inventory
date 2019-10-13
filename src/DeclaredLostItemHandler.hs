module DeclaredLostItemHandler where

import CommonSimpleTypes
import CommonCompoundTypes

import InventorySystemCommands
import DeclaredLostItemPublicTypes

import DeclareLostItemImplementation

import Data.Time

import Data.UUID.V4
import Data.UUID  -- Internal

import Data.Set

import Data.Either.Combinators




-- ==========================================================================
-- This file contains the definitions of PUBLIC types 
-- (exposed at the boundary of the bounded context )
-- related to the Declare LostItem workflow 
-- ==========================================================================



-- =============================================================================
-- IO Dependencies
-- =============================================================================

--- Dependencies 

type LookupOneCategory = 
    String -> Either DbError Category

type LockupAttributes = 
    [(String, String, String)] -> Either DbError [Attribute]

type SaveOneCategory = 
    Category -> Either DbError ()



-- =============================================================================
-- Workflow Dummy Implementations
-- =============================================================================

checkAdministrativeAreaInfoValid :: CheckAdministrativeAreaInfoValid
checkAdministrativeAreaInfoValid (r, d, s) = 
    return (r, d, s)

checkAttributeInfoValid :: CheckAttributeInfoValid   
checkAttributeInfoValid (aco, aca, ct)  =
    return (aco, aca, ct)  

checkContactInfoValid :: CheckContactInfoValid 
checkContactInfoValid t = return t           
    
createDeclarationAcknowledgment :: CreateDeclarationAcknowledgment  
createDeclarationAcknowledgment item = 
    HtmlString "Letter content"
    
sendAcknowledgment :: SendAcknowledgment 
sendAcknowledgment declarationAcknowledgment = 
    Sent  -- DeclarationAcknowledgment -> SendResult             
                      

lookupOneCategory :: LookupOneCategory 
lookupOneCategory catId = 
    do  catid <- mapLeft DbError $ createCategoryId "AAAAAA-HHHHH-JJJJJ-KKKKK-MMMMMM-LLLLL-XXXXX"
        catdesc <- mapLeft DbError $ createLongDescription "Human Category: It captures anything related humans"
        return  Category {
                    categoryId = catid
                ,   categoryType = Humans
                ,   parentalStatus = Parent
                ,   categoryDesc = catdesc
                ,   subCategories = fromList []  
                }

lockupAttributes :: LockupAttributes
lockupAttributes [(acode, catId, catTy)] = Right []

-- =============================================================================
-- Handlers Implementation
-- =============================================================================

declareLostItemHandler :: 
    LookupOneCategory 
    -> LockupAttributes
    -> SaveOneCategory
    -> DeclareLostItemCmd 
    -> UTCTime
    -> UUID
    -> Either DeclareLostItemError [DeclareLostItemEvent]
    
declareLostItemHandler 
    lookupOneCategory
    lockupAttributes
    saveOneCategory
    (Command unvalidatedLostItem curTime userId) 
    declarationTime 
    lostItemUuid = 
     
    do  -- retrieve referenced category
        refCategory <- mapLeft Db $ lookupOneCategory $ uliCategoryId unvalidatedLostItem 
        -- retrieve referenced attribute
        refAttributes <- mapLeft Db $ lockupAttributes $ fmap toAttributeAndCategoryInfo $ uliattributes unvalidatedLostItem
        -- get creation time
        -- Monad transformer here :) declarationTime <- getCurrentTime
        -- get randon uuid 
        -- Another Monad transformer here :) lostItemUuid <- nextRandom
        -- call workflow
        events <- declareLostItem 
                    checkAdministrativeAreaInfoValid  -- Dependency
                    checkAttributeInfoValid           -- Dependency
                    checkContactInfoValid             -- Dependency
                    createDeclarationAcknowledgment   -- Dependency
                    sendAcknowledgment                -- Dependency
                    unvalidatedLostItem               -- Input
                    declarationTime                   -- Input
                    (toString lostItemUuid)
        return events

        


  
  
  
  
  
  
--- Helpers 

toAttributeAndCategoryInfo :: UnvalidatedAttribute -> (String, String, String)
toAttributeAndCategoryInfo u = 
    (uattrCode u, urelatedCategory u, urelatedCategoryType u)





