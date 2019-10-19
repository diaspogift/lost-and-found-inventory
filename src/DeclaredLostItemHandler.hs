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
    -> (String, String, String) 
    -> Either AdminAreaValidationError (Region, Division, SubDivision)

checkAdministrativeAreaInfoValidBase (AdministrativeMap regions) (strReg, strDiv, strSub) = 
    do  reg <- toRegion strReg
        div <- toDivision strDiv
        sub <- toSubDivision strSub

        let singRegion = filter (isRegionItemRegion reg) regions

        case singRegion of
            [RegionItem freg divs] -> 
                let singDivision = filter (isDivisionItemDivision div) divs
                in case singDivision of 
                    [DivisionItem fdiv subs] ->
                        let singSub = filter ( == sub) subs
                        in case singSub of 
                            [fsub] -> Right (freg, fdiv, fsub)
                            _ -> Left "given sub division not found"
                    _ -> Left "given division not found"
            _ -> Left "given region not found"

checkAdministrativeAreaInfoValid :: CheckAdministrativeAreaInfoValid
checkAdministrativeAreaInfoValid = 
    checkAdministrativeAreaInfoValidBase camerounAdministrativeMap


checkAttributeInfoValidBase :: 
    [AttributeRef]
    -> UnvalidatedAttribute 
    -> UnvalidatedLostItem 
    -> Either AttributeValidationError ValidatedAttribute
checkAttributeInfoValidBase refferedAttributes uattr ulositem = 
    do  let foundAttribute = filter (isAttributesEqualTo uattr) refferedAttributes 
        case foundAttribute of
            [attributeRef] -> 
                do  lostItemCatId <- createCategoryId $ uliCategoryId ulositem
                    let maybeCatType = lookup lostItemCatId  (relatedCategoriesRef attributeRef)
                    case maybeCatType of
                        Just _ -> 
                            do  code <- createAttributeCode $ uattrCode uattr
                                name <- createAttributeName $ uattrName uattr
                                desc <- createShortDescription $ uattrDescription uattr
                                valu <- createAttributeValue $ uattrValue uattr
                                unit <- createAttributeUnit $ uattrUnit uattr
                                return  
                                    ValidatedAttribute {
                                            vattrCode = code
                                        ,   vattrName = name
                                        ,   vattrDescription = desc
                                        ,   vattrValue = Just valu
                                        ,   vattrUnit = Just unit
                                        }
                        Nothing -> Left "invalid referenced attribute"
            _ -> Left "referenced attribute not found"


        where isAttributesEqualTo unalidatedAttr attribute =
                    (uattrCode unalidatedAttr) == (unwrapAttributeCode $ attrCodeRef attribute)

    

checkAttributeInfoValid :: CheckAttributeInfoValid   
checkAttributeInfoValid = checkAttributeInfoValidBase [] 

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

