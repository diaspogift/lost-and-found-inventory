{-# LANGUAGE OverloadedStrings #-} -- That library uses `Text` pervasively. This pragma permits to use
                                   -- String literal when a Text is needed.


module DeclaredLostItemHandler where

import CommonSimpleTypes
import CommonCompoundTypes
import CreateRootCategoryDto

import InventorySystemCommands
import DeclaredLostItemPublicTypes

import DeclareLostItemImplementation
import DeclareLostItemDto

import Data.Time
import Data.Maybe


import Data.UUID.V4
import Data.UUID hiding (null) -- Internal

import Data.Set hiding (filter, null)

import Control.Monad.Except

import Data.Int


import Data.Either.Combinators

import Control.Applicative

import EventStore

import Database.EventStore








-- ==========================================================================
-- This file contains the definitions of PUBLIC types 
-- (exposed at the boundary of the bounded context )
-- related to the Declare LostItem workflow 
-- ==========================================================================



-- =============================================================================
-- IO Dependencies types
-- =============================================================================


type LocalStreamId = String

type LookupOneCategory = 
    String -> ExceptT WorkflowError IO Category

type LookupAttributes = 
    [String] -> ExceptT WorkflowError IO [AttributeRef]


type LoadAdministrativeAreaMap =
    String -> ExceptT WorkflowError IO AdministrativeMap

type NextId = IO UnvalidatedLostItemId





-- =============================================================================
-- Workflow dependencies dummy Implementations
-- =============================================================================



--- TODO: Consider using a Tree Structure for the  AdministrativeMap data type
---
---

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
                do  lostItemCatId <- crtCatgrId $ uliCategoryId ulositem
                    let maybeCatType = lookup lostItemCatId  (attributeRefRelatedCategories attributeRef)
                    case maybeCatType of
                        Just _ -> 
                            do  code <- crtAttrCd $ uattrCode uattr
                                name <- crtAttrNm $ uattrName uattr
                                desc <- crtShrtDescpt $ uattrDescription uattr
                                valu <- crtOptAttrVal $ uattrValue uattr
                                unit <- crtOptAttrUnt $ uattrUnit uattr
                                return  
                                    ValidatedAttribute {
                                            vattributeCode = code
                                        ,   vattributeName = name
                                        ,   vattributeDescription = desc
                                        ,   vattributeValue = valu
                                        ,   vattributeUnit = unit
                                        }
                        Nothing -> Left "invalid referenced attribute"
            _ -> Left "referenced attribute not found"


        where isAttributesEqualTo unalidatedAttr attribute =
                    (uattrCode unalidatedAttr) == (uwrpAttrCd $ attributeRefCode attribute)


checkContactInfoValid :: CheckContactInfoValid 
checkContactInfoValid  =  return           
    

crtDeclarationAcknowledgment :: CreateDeclarationAcknowledgment  
crtDeclarationAcknowledgment item = 
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
            Nothing -> liftEither $ mapLeft DataBase $ Left $ DataBaseError "category not found"


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
            Nothing -> liftEither $ mapLeft DataBase $ Left $ DataBaseError "attribute not found"


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




            

-- =============================================================================
-- Declare / Register Lost Item Command Handler Implementation
-- =============================================================================






declareLostItemHandler :: 
    LoadAdministrativeAreaMap
    -> LookupOneCategory 
    -> ReadOneCategory
    -> ReadOneAttributeRef
    -> WriteDeclaredLostItemEvents
    -> NextId
    -> DeclareLostItemCmd 
    -> ExceptT WorkflowError IO [DeclareLostItemEvent]
    
declareLostItemHandler 
    loadAdministrativeAreaMap
    lookupOneCategory
    readOneCategory
    readOneAttributeRef
    writeEventsToStore
    nextId
    (Command unvalidatedLostItem curTime userId) = 

        ---------------------------------------- IO at the boundary start -----------------------------------------
     
    do  -- get event store connection // TODO: lookup env ... or Reader Monad ??????
        conn <- liftIO $ connect defaultSettings (Static "localhost" 1113)

        -- retrieve the referenced categoryId
        let strCatgryId = uliCategoryId unvalidatedLostItem

        -- retrieve adminitrative area map 
        adminAreaMap <-  loadAdministrativeAreaMap "Cameroun"
                  

        -- retrieve referenced category from event store
        referencedCatgr <- readOneCategory conn 10 strCatgryId


        -- retrieve referenced attributes
        refAttributes <- traverse (readOneAttributeRef conn 10) $ uattrCode <$> uliattributes unvalidatedLostItem
        
        -- get creation time
        declarationTime <- liftIO getCurrentTime

        -- get randon uuid 
        lostItemUuid <- liftIO nextId

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
                    crtDeclarationAcknowledgment        -- Dependency
                    sendAcknowledgment                  -- Dependency
                    referencedCatgr                     -- Input
                    unvalidatedLostItem                 -- Input
                    declarationTime                     -- Input
                    lostItemUuid                        -- Input

        ---------------------------------------- Core business logic end ----------------------------------------






        ---------------------------------------- Side effects handling start ----------------------------------------

        -- publish / persit event(s) into the event store and other interested third parties 
        case events of  
            Right allEvents -> 
                do
                    let declLostItemEvts = filter persistableEvts allEvents
                    res <- liftIO $ writeEventsToStore  conn lostItemUuid declLostItemEvts
                    liftEither events
            Left errorMsg -> liftEither $ Left errorMsg

            where persistableEvts (LostItemDeclared _) = True
                  persistableEvts (LocationsAdded _) = True
                  persistableEvts (AttributesAdded _) = True
                  persistableEvts _ = False

        ---------------------------------------- Side effects handling end ----------------------------------------



--- partially applied function for the API (Upper) layer - hinding depencies 
---
---

publicDeclareLostItemHandler :: DeclareLostItemCmd -> ExceptT WorkflowError IO [DeclareLostItemEvent]
publicDeclareLostItemHandler = 
    declareLostItemHandler 
        loadAdministrativeAreaMap
        lookupOneCategory
        readOneCategory
        readOneAttributeRef
        writeDeclaredLostItemEvents
        nextId

        
 