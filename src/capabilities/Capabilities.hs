module Capabilities where

import Authentication (
    IPrincipal
    )
import Authorization (
    onlyForSameId, 
    onlyForAdmins
    )
import Common.CompoundTypes (
    Category, 
    AttributeRef
    )
import Common.SimpleTypes (
    LostItemId, 
    UserId, 
    CategoryId, 
    AttributeCode, 
    WorkflowError
    )
import Data.Int (
    Int32
    )
import DeclareLostItemPublicTypes (
    DeclaredLostItem
    )
import EventStore (
    readOneDeclaredLostItem
    )




-- =============================================================================
-- This file defines the capabilities in the system
-- =============================================================================




-- -----------------------------------------------------------------------------
-- Capability types
-- -----------------------------------------------------------------------------




type ReadCategoryStreamCap = () -> IO (Either WorkflowError Category)

type WriteCategoryEventsCap = () -> IO ()

type ReadAttributeStreamCap = () -> IO (Either WorkflowError AttributeRef)

type WriteAttributeEventsCap = () -> IO ()

type ReadDeclaredLostItemStreamCap = () -> IO (Either WorkflowError DeclaredLostItem)

type WriteDeclaredLostItemEventsCap = () -> IO ()




-- -----------------------------------------------------------------------------
-- Capability providers
-- -----------------------------------------------------------------------------




data CapabilityProvider
  = CapabilityProvider
      { -- given a userId and IPrincipal, attempt to get the ReadCategoryStream capability
        readCategoryStreamCap :: CategoryId -> IPrincipal -> Maybe ReadCategoryStreamCap,
        -- given a userId and IPrincipal, attempt to get the WriteCategoryEvents capability
        writeCategoryEventsCap :: UserId -> IPrincipal -> Maybe WriteCategoryEventsCap,
        -- given a userId and IPrincipal, attempt to get the ReadAttributeStream capability
        readAttributeStreamCap :: AttributeCode -> IPrincipal -> Maybe ReadAttributeStreamCap,
        -- given a userId and IPrincipal, attempt to get the WriteAttributeEvents capability
        writeAttributeEventsCap :: UserId -> IPrincipal -> Maybe WriteAttributeEventsCap,
        -- given a userId and IPrincipal, attempt to get the ReadDeclaredLostItemStreamC capability
        readDeclaredLostItemStreamCap :: LostItemId -> IPrincipal -> Maybe ReadDeclaredLostItemStreamCap,
        -- given a userId and IPrincipal, attempt to get the writeDeclaredLostItemEventsCap capability
        writeDeclaredLostItemEventsCap :: UserId -> IPrincipal -> Maybe WriteDeclaredLostItemEventsCap
      }




getLostItemOnlyForSameId :: 
    Int32 
    -> LostItemId 
    -> IPrincipal 
    -> Maybe (() -> IO (Either WorkflowError DeclaredLostItem))
getLostItemOnlyForSameId numEvts id principal 
    = onlyForSameId id principal (readOneDeclaredLostItem numEvts)



    
getLostItemOnlyForAdmins :: 
    Int32 
    -> LostItemId 
    -> IPrincipal 
    -> Maybe (() -> IO (Either WorkflowError DeclaredLostItem))
getLostItemOnlyForAdmins numEvts id principal
    = onlyForAdmins id principal (readOneDeclaredLostItem numEvts)
