module Authorization where

import Authentication (
    IPrincipal, 
    Role (..), 
    lostItemOwnedByPrincipal, 
    checkPrincipalIsInRole
    )
import Common.SimpleTypes (
    LostItemId
    )




onlyForSameId :: LostItemId -> IPrincipal -> (LostItemId -> a) -> Maybe (() -> a)
onlyForSameId lostItemId principal ƒ =
  if lostItemOwnedByPrincipal lostItemId principal
    then Just (\() -> ƒ lostItemId)
    else Nothing



    
onlyForAdmins :: LostItemId -> IPrincipal -> (LostItemId -> a) -> Maybe (() -> a)
onlyForAdmins lostItemId principal ƒ =
  if checkPrincipalIsInRole principal Administrator
    then Just (\() -> ƒ lostItemId)
    else Nothing
