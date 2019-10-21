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
    String -> EitherIO DeclareLostItemError Category

type LookupAttributes = 
    [String] -> EitherIO DeclareLostItemError [AttributeRef]

type WriteEvent = 
    Connection -> DeclareLostItemEvent -> IO ()


type LoadAdministrativeAreaMap =
    String -> EitherIO DeclareLostItemError AdministrativeMap

type NextId = IO UnvalidatedLostItemId





-- =============================================================================
-- Workflow dependencies dummy Implementations
-- =============================================================================




checkAdministrativeAreaInfoValid :: 
    AdministrativeMap
    -> CheckAdministrativeAreaInfoValid
checkAdministrativeAreaInfoValid (AdministrativeMap regions) (strReg, strDiv, strSub)  
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



checkAttributeInfoValid :: 
    [AttributeRef]
    -> CheckAttributeInfoValid
checkAttributeInfoValid refferedAttributes uattr ulositem = 
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
            Just category -> liftEither $ Right category
            Nothing -> liftEither $ mapLeft Db $ Left $ DbError "category not found"


lookupOneCategory :: LookupOneCategory 
lookupOneCategory = lookupOneCategoryBase allCategories 


lookupAttributesBase :: 
    [(String, AttributeRef)]
    -> LookupAttributes
lookupAttributesBase attributeRefs attrCodes =
    do  let maybeAttributeRefs =   sequence $ recursiveLookup attrCodes attributeRefs
        --print maybeAttributeRefs
        case maybeAttributeRefs of
            Just attributes -> liftEither $ Right attributes
            Nothing -> liftEither $ mapLeft Db $ Left $ DbError "attribute not found"


recursiveLookup :: [String] -> [(String, AttributeRef)] -> [Maybe AttributeRef]
recursiveLookup [] _ = []
recursiveLookup (x:xs) attrRefs = (lookup x attrRefs) : (recursiveLookup xs attrRefs)

                
lookupAttributes :: LookupAttributes
lookupAttributes = lookupAttributesBase allAttributes


loadAdministrativeAreaMap :: LoadAdministrativeAreaMap
loadAdministrativeAreaMap country = 
    liftEither $ Right camerounAdministrativeMap


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
    -> EitherIO DeclareLostItemError [DeclareLostItemEvent]
    
declareLostItemHandler 
    loadAdministrativeAreaMap
    lookupOneCategory
    lookupAttributes
    writeEventToStore
    nextId
    (Command unvalidatedLostItem curTime userId) = 

        ---------------------------------------- IO at the boundary start -----------------------------------------
     
    do  -- get event store connection // TODO: lookup env ... or Reader Monad ??????
        conn <- liftIO $ connect defaultSettings (Static "localhost" 1113)

        -- retrieve adminitrative area map 
        adminAreaMap <-  loadAdministrativeAreaMap "Cameroun"

        -- retrieve referenced category
        refCategory <- lookupOneCategory 
                            $ uliCategoryId unvalidatedLostItem

        -- retrieve referenced attributes
        refAttributes <- lookupAttributes 
                            $ fmap uattrCode 
                            $ uliattributes unvalidatedLostItem

        -- get creation time
        declarationTime <- liftIO $ getCurrentTime

        -- get randon uuid 
        lostItemUuid <- liftIO $ nextId

        -- Arranging final dependencies
        let checkAdministrativeArea = 
                checkAdministrativeAreaInfoValid adminAreaMap
        let checkAttributeInfo =
                checkAttributeInfoValid refAttributes

        ---------------------------------------- IO at the boundary end -----------------------------------------
    
        


        
        ---------------------------------------- Core business logic start ----------------------------------------

        -- call workflow
        let events =
                declareLostItem 
                    checkAdministrativeArea             -- Dependency
                    checkAttributeInfo                  -- Dependency
                    checkContactInfoValid               -- Dependency
                    createDeclarationAcknowledgment     -- Dependency
                    sendAcknowledgment                  -- Dependency
                    unvalidatedLostItem                 -- Input
                    declarationTime                     -- Input
                    lostItemUuid                        -- Input

        ---------------------------------------- Core business logic end ----------------------------------------






        ---------------------------------------- Side effects handling start ----------------------------------------

        -- publish / persit event(s) into the event store
        case events of 
            Right allEvents -> 
                do
                    let declLostItemEvt = filter isDeclLostItemEvent allEvents
                        evt = declLostItemEvt!!0
                    res <- liftIO $ writeEventToStore conn evt
                    -- print declLostItemEvt

                    liftEither events
            Left errorMsg -> liftEither $ Left errorMsg

            where isDeclLostItemEvent (LostItemDeclared lostItemDeclared) = True
                  isDeclLostItemEvent _ = False

        ---------------------------------------- Side effects handling end ----------------------------------------




publicDeclareLostItemHandler :: 
    DeclareLostItemCmd 
    -> EitherIO DeclareLostItemError [DeclareLostItemEvent]
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

liftEither :: Either e a -> EitherIO e a
liftEither x = EitherIO (return x)
  
liftIO :: IO a -> EitherIO e a
liftIO x = EitherIO (fmap Right x)