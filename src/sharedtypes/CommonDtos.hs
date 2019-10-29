
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}



module CommonDtos where


import Data.Aeson
import CommonSimpleTypes
import CommonCompoundTypes
import CreateAttributePublicTypes


import Prelude hiding (last, id)
import Data.Time
import Data.Set hiding (null, singleton)
import Data.Map hiding (null, toList)

import GHC.Generics







-- ==========================================================================================
-- This file contains the the logic for working with common data transfer objects (DTOs)
--
-- 
--
-- ==========================================================================================




-- ==========================================================================================
-- DTOs for common workflow types
-- ==========================================================================================



-- ----------------------------------------------------------------------------
-- DTO for worflow errors
-- ----------------------------------------------------------------------------




data WorkflowErrorDto = WorkflowErrorDto {
        code :: String
    ,   message :: String
    } deriving (Generic, Show)

instance ToJSON WorkflowErrorDto 

instance FromJSON WorkflowErrorDto



-- Helper functions for converting from / to domain as well as to other states

fromWorkflowError :: WorkflowError -> WorkflowErrorDto
fromWorkflowError domainError = 
    case domainError of
        Validation (ValidationError errorMessage) ->
            WorkflowErrorDto {
                code = "ValidationError"
            ,   message = errorMessage
            }
        Domain (DomainError errorMessage) ->
            WorkflowErrorDto {
                code = "DomainError"
            ,   message = errorMessage
            }
        Remote remoteServiceError ->
            let serv = service remoteServiceError 
                httpCd = errorCode remoteServiceError
                errorMessage = execption remoteServiceError
            in WorkflowErrorDto {
                code = show httpCd
            ,   message = serviceName serv  
            }
        DataBase (DataBaseError errorMessage) ->
            WorkflowErrorDto {
                    code = "DataBaseError"
                ,   message = errorMessage
                } 


