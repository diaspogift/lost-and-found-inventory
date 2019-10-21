{-# LANGUAGE OverloadedStrings #-} -- That library uses `Text` pervasively. This pragma permits to use
                                   -- String literal when a Text is needed.


module DeclaredLostItemHandler where

import CommonSimpleTypes
import CommonCompoundTypes

import InventorySystemCommands
import DeclaredLostItemPublicTypes

import DeclareLostItemImplementation
import DeclareLostItemDto

import Data.Time

import Data.Text (pack)

import Data.UUID.V4
import Data.UUID hiding (null) -- Internal

import Data.Set hiding (filter, null)

import Data.Either.Combinators

import Control.Applicative


import Control.Concurrent.Async 

import Database.EventStore


import Data.Aeson




-- ==========================================================================
-- This file contains the definitions of PUBLIC types 
-- (exposed at the boundary of the bounded context )
-- related to the Declare LostItem workflow 
-- ==========================================================================



-- =============================================================================
-- IO Dependencies types
-- =============================================================================




type LookupOneCategory = 
    String -> IO (Either DbError Category)

type LookupAttributes = 
    [String] -> IO (Either DbError [AttributeRef])

type WriteEvent = 
    Connection -> DeclareLostItemEvent -> IO ()

type WriteEvent1 = 
    DeclareLostItemEvent -> IO ()

type LoadAdministrativeAreaMap =
    String -> IO (Either DbError AdministrativeMap)

type NextId = IO UnvalidatedLostItemId





-- =============================================================================
-- Workflow dependencies dummy Implementations
-- =============================================================================




checkAdministrativeAreaInfoValidBase :: 
    AdministrativeMap
    -> CheckAdministrativeAreaInfoValid
checkAdministrativeAreaInfoValidBase (AdministrativeMap regions) (strReg, strDiv, strSub)  
    | null strReg && null strDiv && null strSub = 
        Right Nothing
    | otherwise =
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
                                [fsub] -> Right $ Just (freg, fdiv, fsub)
                                _ -> Left "given sub division not found"
                        _ -> Left "given division not found"
                _ -> Left "given region not found"


checkAdministrativeAreaInfoValid :: CheckAdministrativeAreaInfoValid
checkAdministrativeAreaInfoValid = checkAdministrativeAreaInfoValidBase camerounAdministrativeMap

checkAttributeInfoValidBase :: 
    [AttributeRef]
    -> CheckAttributeInfoValid
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
                                valu <- createOptionalAttributeValue $ uattrValue uattr
                                unit <- createOptionalAttributeUnit $ uattrUnit uattr
                                return  
                                    ValidatedAttribute {
                                            vattrCode = code
                                        ,   vattrName = name
                                        ,   vattrDescription = desc
                                        ,   vattrValue = valu
                                        ,   vattrUnit = unit
                                        }
                        Nothing -> Left "invalid referenced attribute"
            _ -> Left "referenced attribute not found"


        where isAttributesEqualTo unalidatedAttr attribute =
                    (uattrCode unalidatedAttr) == (unwrapAttributeCode $ attrCodeRef attribute)


checkAttributeInfoValid :: CheckAttributeInfoValid   
checkAttributeInfoValid = checkAttributeInfoValidBase attributes


checkContactInfoValid :: CheckContactInfoValid 
checkContactInfoValid  =  return           
    

createDeclarationAcknowledgment :: CreateDeclarationAcknowledgment  
createDeclarationAcknowledgment item = 
    HtmlString "Letter content"
    

sendAcknowledgment :: SendAcknowledgment 
sendAcknowledgment declarationAcknowledgment = 
    Sent  -- DeclarationAcknowledgment -> SendResult             
        
    
lookupOneCategoryBase :: 
    [(String, Category)] -> LookupOneCategory
lookupOneCategoryBase categories categoryId = 
    do  let maybeCategory = lookup categoryId categories
        --print maybeCategory
        case maybeCategory of
            Just category -> return $ Right category
            Nothing -> return $ Left $ DbError "category not found"


lookupOneCategory :: LookupOneCategory 
lookupOneCategory = lookupOneCategoryBase allCategories 


lookupAttributesBase :: 
    [(String, AttributeRef)]
    -> LookupAttributes
lookupAttributesBase attributeRefs attrCodes =
    do  let maybeAttributeRefs =   sequence $ recursiveLookup attrCodes attributeRefs
        --print maybeAttributeRefs
        case maybeAttributeRefs of
            Just attributes -> return $ Right attributes
            Nothing -> return $ Left $ DbError "attribute not found"


recursiveLookup :: [String] -> [(String, AttributeRef)] -> [Maybe AttributeRef]
recursiveLookup [] _ = []
recursiveLookup (x:xs) attrRefs = (lookup x attrRefs) : (recursiveLookup xs attrRefs)

                
lookupAttributes :: LookupAttributes
lookupAttributes = lookupAttributesBase allAttributes


loadAdministrativeAreaMap :: LoadAdministrativeAreaMap
loadAdministrativeAreaMap country = 
    return $ Right camerounAdministrativeMap


nextId :: NextId
nextId = 
    let id = nextRandom in fmap toString id




writeEventToStore :: WriteEvent
writeEventToStore conn (LostItemDeclared lostItemDeclared) = 
    do  let lostItemDeclaredDto = fromLostItemDeclared lostItemDeclared
            lostItemDeclaredEvent = createEvent "LostItemDeclared" Nothing $ withJson lostItemDeclaredDto
            id = dtoitemId lostItemDeclaredDto
        as <- sendEvent conn (StreamName $ pack ( "lost-item-id-: " <> id)) anyVersion lostItemDeclaredEvent Nothing
        _  <- wait as
        shutdown conn
        waitTillClosed conn

{--

writeEventToStore :: WriteEvent
writeEventToStore conn event = 
    do
        case event of
            LostItemDeclared lostItemDeclared
                ->     let  lostItemDeclaredDto = fromLostItemDeclared lostItemDeclared
                            lostItemDeclaredEvent = createEvent "LostItemDeclared" Nothing $ withJson lostItemDeclaredDto
                            id = dtoitemId lostItemDeclaredDto
                        in do
                            as <- sendEvent conn (StreamName $ pack id) anyVersion lostItemDeclaredEvent Nothing
                            _  <- wait as
                            shutdown conn
                            waitTillClosed conn
                            return ()
            _ 
                -> return ()

--}

            

-- =============================================================================
-- Declare / Register Lost Item Command Handler Implementation
-- =============================================================================




---- TODO: NEEDS LOTS OF IMPROVEMENTS
---- 1- Transform IO (Either DeclareLostItemError [DeclareLostItemEvent]) into EitherIO DeclareLostItemError [DeclareLostItemEvent] 

declareLostItemHandler :: 
    LoadAdministrativeAreaMap
    -> LookupOneCategory 
    -> LookupAttributes
    -> WriteEvent
    -> NextId
    -> DeclareLostItemCmd 
    -> IO (Either DeclareLostItemError [DeclareLostItemEvent])
    
declareLostItemHandler 
    loadAdministrativeAreaMap
    lookupOneCategory
    lookupAttributes
    writeEventToStore
    nextId
    (Command unvalidatedLostItem curTime userId) = 
     
    do  -- get event store connection // TODO: lookup env ... or Reader Monad ??????
        conn <- connect defaultSettings (Static "localhost" 1113)

        -- retrieve adminitrative area map 
        adminAreaMap <- loadAdministrativeAreaMap "Cameroun"

        -- retrieve referenced category
        refCategory <- lookupOneCategory 
                            $ uliCategoryId unvalidatedLostItem

        -- retrieve referenced attributes
        refAttributes <- lookupAttributes 
                            $ fmap uattrCode 
                            $ uliattributes unvalidatedLostItem

        -- get creation time
        declarationTime <- getCurrentTime

        -- get randon uuid 
        lostItemUuid <- nextId

        -- call workflow
        let events =
                declareLostItem 
                    checkAdministrativeAreaInfoValid  -- Dependency
                    checkAttributeInfoValid           -- Dependency
                    checkContactInfoValid             -- Dependency
                    createDeclarationAcknowledgment   -- Dependency
                    sendAcknowledgment                -- Dependency
                    unvalidatedLostItem               -- Input
                    declarationTime                   -- Input
                    lostItemUuid                      -- Input

        
        -- publish / persit event into the event store
        case events of 
            Right allEvents -> 
                do
                    let declLostItemEvt = filter isDeclLostItemEvent allEvents
                        evt = declLostItemEvt!!0
                    res <- writeEventToStore conn evt
                    print declLostItemEvt

                    return events
            Left errorMsg -> return $ Left errorMsg

            where isDeclLostItemEvent (LostItemDeclared lostItemDeclared) = True
                  isDeclLostItemEvent _ = False


publicDeclareLostItemHandler :: 
    DeclareLostItemCmd 
    -> IO (Either DeclareLostItemError [DeclareLostItemEvent])
publicDeclareLostItemHandler = 
    declareLostItemHandler 
        loadAdministrativeAreaMap
        lookupOneCategory
        lookupAttributes
        writeEventToStore
        nextId

        
   



---- TODO: Transform     IO (Either e a)  into EitherIO e a  

---- Getting thereeee :) - This is Monad trasformer LANNNNNNNNDDDD!!!  



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

