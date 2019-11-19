{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Dtos where

import Common.SimpleTypes (
    WorkflowError (..),
    ValidationError (..),
    DomainError (..),
    DataBaseError (..),
    service,
    errorCode,
    execption,
    serviceName
    )
import Data.Aeson (
    ToJSON,
    FromJSON
    )
import GHC.Generics (
    Generic
    )   




-- ==========================================================================================
--
-- This file contains the the logic for working with common data transfer objects (DTOs)
--
-- ==========================================================================================




-- ==========================================================================================
-- DTOs for common workflow types
-- ==========================================================================================




-- ----------------------------------------------------------------------------
-- DTO for worflow errors
-- ----------------------------------------------------------------------------




data WorkflowErrorDto
  = WorkflowErrorDto
      { worflowErrorCde :: String,
        worflowErrorMsg :: String
      }
  deriving (Generic, Show)

instance ToJSON WorkflowErrorDto
instance FromJSON WorkflowErrorDto




fromWorkflowError :: WorkflowError -> WorkflowErrorDto
fromWorkflowError domainError =
  case domainError of
    Validation (ValidationError errorMessage) ->
      WorkflowErrorDto
        { worflowErrorCde = "ValidationError",
          worflowErrorMsg = errorMessage
        }
    Domain (DomainError errorMessage) ->
      WorkflowErrorDto
        { worflowErrorCde = "DomainError",
          worflowErrorMsg = errorMessage
        }
    Remote remoteServiceError ->
      let serv = service remoteServiceError
          httpCd = errorCode remoteServiceError
          errorMessage = execption remoteServiceError
       in WorkflowErrorDto
            { worflowErrorCde = show httpCd,
              worflowErrorMsg = serviceName serv
            }
    DataBase (DataBaseError errorMessage) ->
      WorkflowErrorDto
        { worflowErrorCde = "DataBaseError",
          worflowErrorMsg = errorMessage
        }




        
data AddedSubCategoryDto
  = AddedSubCategoryDto
      { parent :: String,
        sub :: String
      }
  deriving (Generic, Show)

instance ToJSON AddedSubCategoryDto
instance FromJSON AddedSubCategoryDto
