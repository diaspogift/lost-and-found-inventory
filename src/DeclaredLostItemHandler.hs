module DeclaredLostItemHandler where

import CommonSimpleTypes
import CommonCompoundTypes

import InventorySystemCommands
import DeclaredLostItemPublicTypes

import DeclareLostItemImplementation

import Data.Time

import Data.UUID.V4
import Data.UUID  -- Internal

import Data.Set hiding (filter)

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

type NextId = IO UnvalidatedLostItemId





-- =============================================================================
-- Workflow dependencies Dummy Implementations
-- =============================================================================

checkAdministrativeAreaInfoValidBase :: 
    AdministrativeMap
    -> (Region, Division, SubDivision) 
    -> Either AdminAreaValidationError (Region, Division, SubDivision)

checkAdministrativeAreaInfoValidBase (AdministrativeMap regions) (aRegion, aDivision, aSubDivision) = 
    let singRegion = filter (isRegionItemRegion aRegion) regions
    in case singRegion of
        [RegionItem freg divs] -> 
            let singDivision = filter (isDivisionItemDivision aDivision) divs
            in case singDivision of 
                [DivisionItem fdiv subs] ->
                    let singSub = filter ( == aSubDivision) subs
                    in case singSub of 
                        [fsub] -> Right (freg, fdiv, fsub)
                        _ -> Left "given sub division not found"
                _ -> Left "given division not found"
        _ -> Left "given region not found"

checkAdministrativeAreaInfoValid :: CheckAdministrativeAreaInfoValid
checkAdministrativeAreaInfoValid (r, d, s) = 
    return (r, d, s)


checkAttributeInfoValid :: CheckAttributeInfoValid   
checkAttributeInfoValid = undefined 

checkContactInfoValid :: CheckContactInfoValid 
checkContactInfoValid  =  return           
    
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
                                    ,   enablementStatus = Enabled
                                    ,   categoryDesc = desc
                                    ,   subCategories = fromList []  
                                    }

lockupAttributes :: LockupAttributes
lockupAttributes [(acode, catId, catTy)] = return $ Right []

loadAdministrativeAreaMap :: LoadAdministrativeAreaMap
loadAdministrativeAreaMap country = 
    return $ Right camerounAdministrativeMap

nextId :: NextId
nextId = let id = nextRandom in fmap toString id


-- =============================================================================
-- Declare / Register Lost Item Command Handler Implementation
-- =============================================================================

---- TODO: NEEDS LOTS OF IMPROVEMENTS

declareLostItemHandler :: 
    LookupOneCategory 
    -> LockupAttributes
    -> SaveOneCategory
    -> NextId
    -> DeclareLostItemCmd 
    -> IO (Either DeclareLostItemError [DeclareLostItemEvent])
    
declareLostItemHandler 
    lookupOneCategory
    lockupAttributes
    saveOneCategory
    nextId
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
        lostItemUuid <- nextId
        -- call workflow
        return $
            declareLostItem 
                checkAdministrativeAreaInfoValid  -- Dependency
                checkAttributeInfoValid           -- Dependency
                checkContactInfoValid             -- Dependency
                createDeclarationAcknowledgment   -- Dependency
                sendAcknowledgment                -- Dependency
                unvalidatedLostItem               -- Input
                declarationTime                   -- Input
                lostItemUuid

        
   



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

