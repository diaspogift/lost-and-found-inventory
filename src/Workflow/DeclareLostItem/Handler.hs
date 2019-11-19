{-# LANGUAGE OverloadedStrings #-}

-- That library uses `Text` pervasively. This pragma permits to use
-- String literal when a Text is needed.

module Workflow.DeclareLostItem.Handler where

import Common.CompoundTypes
import Common.SimpleTypes
import Control.Monad.Except
    (ExceptT (..),
    liftEither,
    liftIO
    )
import Data.Char 
    (toUpper)
import Data.Time 
    (getCurrentTime)
import Data.UUID 
    (toString)
import Data.UUID.V4 
    (nextRandom)
import Database.EventStore
import Workflow.DeclareLostItem.Implementation
import Workflow.DeclareLostItem.PublicTypes
import Persistence.EventStore 
    (ReadOneCategory,
    ReadOneAttributeRef,
    WriteDeclaredLostItemEvents,
    readOneCategory,
    readOneAttributeRef,
    writeDeclaredLostItemEvents
    )
import Inventory.System.Commands 
    (InventoryCommand (..),
    DeclareLostItemCmd
    )




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



checkAdministrativeAreaInfoValid ::AdministrativeMap -> CheckAdministrativeAreaInfoValid
checkAdministrativeAreaInfoValid (AdministrativeMap regions) (strReg, strDiv, strSub)
    | null strReg && null strDiv && null strSub = Right Nothing
    | otherwise = do reg <- toRegion strReg
                     div <- toDivision strDiv
                     sub <- toSubDivision strSub
                     let singRegion = filter (isRegionItemRegion reg) regions
                     case singRegion of
                            [RegionItem freg divs] ->
                                let singDivision = filter (isDivisionItemDivision div) divs
                                in case singDivision of
                                    [DivisionItem fdiv subs] ->
                                        let singSub = filter (== sub) subs
                                        in case singSub of
                                            [fsub] -> Right $ Just (freg, fdiv, fsub)
                                            _ -> Left "given sub division not found"
                                    _ -> Left "given division not found"
                            _ -> Left "given region not found"




checkAttributeInfoValid ::[AttributeRef] -> CheckAttributeInfoValid
checkAttributeInfoValid refferedAttributes uattr ulositem =
  do let foundAttribute = filter (isAttributesEqualTo uattr) refferedAttributes
     case foundAttribute of
      [attributeRef] ->
        do lostItemCatId <- crtCatgrId $ uliCategoryId ulositem
           let maybeCatType = lookup lostItemCatId . attributeRefRelatedCategories $ attributeRef
           case maybeCatType of
            Just _ -> do
                code <- crtAttrCd $ uattrCode uattr
                name <- crtAttrNm $ uattrName uattr
                desc <- crtShrtDescpt $ uattrDescription uattr
                valu <- crtOptAttrVal $ uattrValue uattr
                unit <- crtOptAttrUnt $ uattrUnit uattr
                return ValidatedAttribute
                  { vattributeCode = code,
                    vattributeName = name,
                    vattributeDescription = desc,
                    vattributeValue = valu,
                    vattributeUnit = unit
                  }
            Nothing -> Left "invalid referenced attribute"
      _ -> Left "referenced attribute not found"
  where isAttributesEqualTo uvattr attr = uattrCode uvattr == (uwrpAttrCd . attributeRefCode $ attr)
 




checkContactInfoValid :: CheckContactInfoValid
checkContactInfoValid = return -- TODO:  I need a proper phone validator service




crtDeclarationAcknowledgment :: CreateDeclarationAcknowledgment
crtDeclarationAcknowledgment item =
  HtmlString "Letter content"




sendAcknowledgment :: DeclarationAcknowledgment -> SendResult
sendAcknowledgment _ =
  Sent




loadAdministrativeAreaMap :: LoadAdministrativeAreaMap
loadAdministrativeAreaMap _ =
  liftEither $ Right camerounAdministrativeMap




nextId :: NextId
nextId = let randomId = nextRandom in fmap (fmap toUpper . toString) randomId





-- =============================================================================
-- Declare / Register Lost Item Command Handler Implementation
-- =============================================================================





declareLostItemHandler :: LoadAdministrativeAreaMap 
                       -> ReadOneCategory 
                       -> ReadOneAttributeRef 
                       -> WriteDeclaredLostItemEvents 
                       -> NextId 
                       -> DeclareLostItemCmd 
                       -> ExceptT WorkflowError IO [DeclareLostItemEvent]
declareLostItemHandler  loadAdministrativeAreaMap
                        readOneCategory
                        readOneAttributeRef
                        writeDeclaredLostItemEvents
                        nextId
                        (Command unvalidatedLostItem curTime userId) =
                        
   

    do let strCatgryId = uliCategoryId unvalidatedLostItem
       adminAreaMap <- loadAdministrativeAreaMap "Cameroun"
       referencedCatgr  <- ExceptT . liftIO $ readOneCategory 10 strCatgryId       
       refAttributes <- ExceptT . liftIO 
                                . fmap sequence 
                                . traverse (readOneAttributeRef 10) 
                                $ uattrCode <$> uliattributes unvalidatedLostItem
       declarationTime <- liftIO getCurrentTime
       lostItemUuid <- liftIO nextId
       let checkAdministrativeArea = checkAdministrativeAreaInfoValid adminAreaMap
       let checkAttributeInfo = checkAttributeInfoValid refAttributes    
       let lostItemIdentifier = lostItemUuid <> ":" <> userId
         
       let events = declareLostItem checkAdministrativeArea 
                                    checkAttributeInfo 
                                    checkContactInfoValid 
                                    crtDeclarationAcknowledgment 
                                    sendAcknowledgment 
                                    referencedCatgr 
                                    unvalidatedLostItem 
                                    declarationTime 
                                    lostItemIdentifier 
       case events of Right allEvents ->
                        do  let declLostItemEvts = filter persistableEvts allEvents
                            _ <- liftIO $ writeDeclaredLostItemEvents lostItemIdentifier
                                                                      declLostItemEvts
                            liftEither events
                      Left errorMsg -> liftEither $ Left errorMsg
                      where persistableEvts (LostItemDeclared _) = True
                            persistableEvts (LocationsAdded _) = True
                            persistableEvts (AttributesAdded _) = True
                            persistableEvts _ = False





--- partially applied function for the API (Upper) layer - hiding depencies
---
---



publicDeclareLostItemHandler :: DeclareLostItemCmd 
                             -> ExceptT WorkflowError IO [DeclareLostItemEvent]
publicDeclareLostItemHandler = declareLostItemHandler loadAdministrativeAreaMap
                                                      readOneCategory
                                                      readOneAttributeRef
                                                      writeDeclaredLostItemEvents
                                                      nextId
                                                            
