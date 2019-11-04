module Capabilities where

import CommonSimpleTypes
import CommonCompoundTypes
import Authentication



-- =============================================================================
-- This file defines the capabilities in the system
-- =============================================================================





-- -----------------------------------------------------------------------------
--
-- Capability types        
--
-- -----------------------------------------------------------------------------


type ReadCategoryStreamCap = () -> IO (Either WorkflowError Category)
type WriteCategoryEventsCap = () -> IO ()

type ReadAttributeStreamCap = () -> IO (Either WorkflowError AttributeRef)
type WriteAttributeEventsCap = () -> IO ()

type ReadDeclaredLostItemStreamCap = () -> IO (Either WorkflowError AttributeRef)
type WriteDeclaredLostItemEventsCap = () -> IO ()


-- -----------------------------------------------------------------------------
--
-- Capability providers        
--
-- -----------------------------------------------------------------------------

data CapabilityProvider = CapabilityProvider {
        -- given a userId and IPrincipal, attempt to get the ReadCategoryStream capability
        readCategoryStreamCap :: CategoryId -> IPrincipal -> Maybe ReadCategoryStreamCap

        -- given a userId and IPrincipal, attempt to get the WriteCategoryEvents capability
    ,   writeCategoryEventsCap :: UserId -> IPrincipal -> Maybe WriteCategoryEventsCap 

        -- given a userId and IPrincipal, attempt to get the ReadAttributeStream capability
    ,   readAttributeStreamCap :: AttributeCode -> IPrincipal -> Maybe ReadAttributeStreamCap

            -- given a userId and IPrincipal, attempt to get the WriteAttributeEvents capability
    ,   writeAttributeEventsCap :: UserId -> IPrincipal -> Maybe WriteAttributeEventsCap

            -- given a userId and IPrincipal, attempt to get the ReadDeclaredLostItemStreamC capability
    ,   readDeclaredLostItemStreamCap :: LostItemId -> IPrincipal -> Maybe ReadDeclaredLostItemStreamCap

            -- given a userId and IPrincipal, attempt to get the writeDeclaredLostItemEventsCap capability
    ,   writeDeclaredLostItemEventsCap :: UserId -> IPrincipal -> Maybe WriteDeclaredLostItemEventsCap
    }