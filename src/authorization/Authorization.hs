module Authorization where 

 


import CommonSimpleTypes
import Authentication


onlyForSameId :: LostItemId -> IPrincipal -> (LostItemId -> a) -> Maybe (() -> a)
onlyForSameId lostItemId principal ƒ = 
    if lostItemOwnedByPrincipal lostItemId principal 
    then Just $ \() -> ƒ lostItemId
    else Nothing


 
onlyForAdmins :: LostItemId -> IPrincipal -> (LostItemId -> a) ->  Maybe (() -> a)
onlyForAdmins lostItemId principal ƒ =
    if checkPrincipalIsInRole principal Administrator
    then Just $ \() -> ƒ lostItemId 
    else Nothing
        