module Authentication where

import Common.SimpleTypes
  ( DomainError (..),
    LostItemId,
    UserId,
    crtUsrId,
    uwrpLstItmId,
    mapDomainError
  )
import Data.Either.Combinators
import Data.List.Split




data Role
  = Administrator
  | Anonymous
  deriving (Eq, Show, Ord)




data IPrincipal
  = IPrincipal
      { identity :: UserId,
        roles :: [Role]
      }




authenticate :: UserId -> Either DomainError IPrincipal
authenticate id = do
  felixId <- mapDomainError $ crtUsrId "111111111111111111111111111111111111"
  meganId <- mapDomainError $ crtUsrId "222222222222222222222222222222222222"
  makePrincipal id felixId meganId
  where
    makePrincipal :: UserId -> UserId -> UserId -> Either DomainError IPrincipal
    makePrincipal userId user1 user2
      | userId == user1 =
        return $ IPrincipal user1 [Administrator, Anonymous]
      | userId == user2 =
        return $ IPrincipal user2 [Anonymous]
      | otherwise = Left . DomainError $ "Authentication Faillure"




lostItemOwnedByPrincipal :: LostItemId -> IPrincipal -> Bool
lostItemOwnedByPrincipal lostItemId (IPrincipal userId _) =
  case toOwnerId lostItemId of
    Right ownerId -> ownerId == userId
    Left errorMsg -> False
  where
    splitOnColumn = splitOn ":"
    toOwnerId = crtUsrId . last . splitOnColumn . uwrpLstItmId



    
checkPrincipalIsInRole :: IPrincipal -> Role -> Bool
checkPrincipalIsInRole (IPrincipal _ roles) role =
  role `elem` roles
