module CreateSubCategoryHandler where

import CommonCompoundTypes
import CommonSimpleTypes
--import CreateAttributeDto

-- Internal


import Control.Monad.Except 
    (ExceptT (..),
    liftEither,
    liftIO)
import CreateCategoryCommonPublicTypes
import CreateSubCategoryImplementation
    (createSubCatgory)
import CreateSubCategoryPublicTypes
import Data.Char 
    (toUpper)
import Data.UUID 
    (toString)
import Data.UUID.V4 
    (nextRandom)
import Database.EventStore
import EventStore 
    (ReadOneCategory, 
    WriteCreateSubCategoryEvents,
    readOneCategory,
    writeCreateSubCategoryEvents
    )
import InventorySystemCommands 
    (InventoryCommand (..), CreateSubCategoryCmd)





-- ==========================================================================
-- This file contains the definitions of PUBLIC types
-- (exposed at the boundary of the bounded context )
-- related to the Create Attribute Ref workflow
-- ==========================================================================




-- =============================================================================
-- IO Dependencies types
-- =============================================================================




type NextId = IO UnvalidatedCategoryId




-- =============================================================================
-- Workflow dependencies dummy Implementations
-- =============================================================================




nextId :: NextId
nextId = let id = nextRandom in fmap (fmap toUpper . toString) id




-- =============================================================================
-- Create Attribute Ref Command Handler Implementation
-- =============================================================================





createSubCategoryHandler :: ReadOneCategory
                         -> WriteCreateSubCategoryEvents
                         -> NextId 
                         -> CreateSubCategoryCmd 
                         -> ExceptT WorkflowError IO [CreateCategoryEvent]
createSubCategoryHandler readOneCategory 
                         writeEventToStore 
                         nextId
                         (Command unvalidatedSubCategory curTime userId) =
    do
      refSubCatgrs <- ExceptT . liftIO 
                              . fmap sequence 
                              . traverse (readOneCategory 10) 
                              $ usubCatgrRelatedsubCatgrs unvalidatedSubCategory
      let (strPrntCatId, strPrntCatCd) = usubCategoryParentIdandCd unvalidatedSubCategory
      unvalidatedCategoryId <- liftIO nextId
      if notNull strPrntCatId && notNull strPrntCatCd then do
          refParentCategory <- ExceptT $ liftIO $ readOneCategory 10 strPrntCatId
          let events = createSubCatgory refSubCatgrs
                                        (Just refParentCategory)
                                        unvalidatedSubCategory
                                        unvalidatedCategoryId
          case events of Right allEvents -> do
                            _ <- liftIO $ writeCreateSubCategoryEvents unvalidatedCategoryId allEvents
                            liftEither events
                         Left errorMsg -> liftEither $ Left errorMsg
      else do let events = createSubCatgory refSubCatgrs
                                            Nothing
                                            unvalidatedSubCategory
                                            unvalidatedCategoryId
              case events of Right allEvents -> do
                                _ <- liftIO $ writeCreateSubCategoryEvents unvalidatedCategoryId allEvents
                                liftEither events
                             Left errorMsg -> liftEither $ Left errorMsg




            
--- partially applied function for the API (Upper) layer - hiding depencies
---
---




publicCreateSubCategoryHandler :: CreateSubCategoryCmd
                               -> ExceptT WorkflowError IO [CreateCategoryEvent]
publicCreateSubCategoryHandler = createSubCategoryHandler readOneCategory
                                                          writeCreateSubCategoryEvents
                                                          nextId
