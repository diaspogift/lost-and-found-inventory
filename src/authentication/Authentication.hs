module Authentication where 


import CommonSimpleTypes
import Data.Either.Combinators






type Name = String

type Identity = String

data LocalPerson = LocalPerson Name Identity

type LocalUserId = String

data Role =  Admininstrator | Anonymous

data IPrincipal = IPrincipal {
        identity    :: LocalPerson
    ,   roles       :: [Role]
} 




authenticate :: LocalUserId -> Either DomainError IPrincipal
authenticate id 
    | id == "111111111111111111111111111111111111" = 
        return $ IPrincipal (LocalPerson "Felicien Fotio MANFO" "111111111111111111111111111111111111") [Admininstrator, Anonymous]
    | id == "222222222222222222222222222222222222" = 
        return $ IPrincipal (LocalPerson "Megan Amanda HESS" "222222222222222222222222222222222222") [Anonymous]
    | otherwise = Left . DomainError $ "Authentication Faillure"
    

lostItemOwnedByPrincipal :: LocalUserId -> IPrincipal -> Bool
lostItemOwnedByPrincipal lostItemOwnerId (IPrincipal (LocalPerson name id) _) = 
    id == lostItemOwnerId