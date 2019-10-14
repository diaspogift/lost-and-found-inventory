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

import Control.Applicative




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
    String -> IO (Either DbError Category)

type LockupAttributes = 
    [(String, String, String)] -> IO (Either DbError [Attribute])

type SaveOneCategory = 
    Category -> IO (Either DbError ())

type LoadAdministrativeAreaMap =
    String -> IO (Either DbError AdministrativeMap)





-- =============================================================================
-- Workflow Dummy Implementations
-- =============================================================================

checkAdministrativeAreaInfoValid :: CheckAdministrativeAreaInfoValid
checkAdministrativeAreaInfoValid (r, d, s) = 
    return (r, d, s)

checkAttributeInfoValid :: CheckAttributeInfoValid   
checkAttributeInfoValid = undefined 

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
    do  catid <- return $ mapLeft DbError $ createCategoryId "AAAAAA-HHHHH-JJJJJ-KKKKK-MMMMMM-LLLLL-XXXXX"
        catdesc <- return $ mapLeft DbError $ createLongDescription "Human Category: It captures anything related humans"
        case catid of
            Left errorMsg -> return $ Left errorMsg
            Right id ->
                case catdesc of
                    Left errorMsg1 -> return $ Left errorMsg1
                    Right desc -> 
                        return $
                            Right Category {
                                        categoryId = id
                                    ,   categoryType = Humans
                                    ,   parentalStatus = Parent
                                    ,   categoryDesc = desc
                                    ,   subCategories = fromList []  
                                    }

lockupAttributes :: LockupAttributes
lockupAttributes [(acode, catId, catTy)] = return $ Right []

loadAdministrativeAreaMap :: LoadAdministrativeAreaMap
loadAdministrativeAreaMap country = 
    return $ Right camerounAdministrativeMap


-- =============================================================================
-- Declare / Register Lost Item Command Handler Implementation
-- =============================================================================

---- TODO: NEEDS LOTS OF IMPROVEMENTS

declareLostItemHandler :: 
    LookupOneCategory 
    -> LockupAttributes
    -> SaveOneCategory
    -> DeclareLostItemCmd 
    -> IO (Either DeclareLostItemError [DeclareLostItemEvent])
    
declareLostItemHandler 
    lookupOneCategory
    lockupAttributes
    saveOneCategory
    (Command unvalidatedLostItem curTime userId) = 
     
    do  -- retrieve adminitrative map area
        adminAreaMap <- loadAdministrativeAreaMap "Cameroun"
        -- retrieve referenced category
        refCategory <- lookupOneCategory $ uliCategoryId unvalidatedLostItem
        -- retrieve referenced attribute
        -- refAttributes <- lockupAttributes $ fmap toAttributeAndCategoryInfo $ uliattributes unvalidatedLostItem
        -- get creation time
        declarationTime <- getCurrentTime
        -- get randon uuid 
        lostItemUuid <- nextRandom
        -- call workflow
        events <- return $
                     declareLostItem 
                        checkAdministrativeAreaInfoValid  -- Dependency
                        checkAttributeInfoValid           -- Dependency
                        checkContactInfoValid             -- Dependency
                        createDeclarationAcknowledgment   -- Dependency
                        sendAcknowledgment                -- Dependency
                        unvalidatedLostItem               -- Input
                        declarationTime                   -- Input
                        lostItemUuid
        return events

        
   



---- TODO: Transform     IO (Either e a)  into EitherIO e a  

-- Getting thereeee :) - This is Monad trasformer LANDDDD!!!  



data EitherIO e a = EitherIO {
    runEitherIO :: IO (Either e a)
}


--
instance Functor (EitherIO e) where
  fmap f = EitherIO . fmap (fmap f) . runEitherIO

--
instance Applicative (EitherIO e) where
  pure    = EitherIO . return . Right
  f <*> x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)
--
instance Monad (EitherIO e) where
  return  = pure
  x >>= f = EitherIO $ runEitherIO x >>= either (return . Left) (runEitherIO . f)

