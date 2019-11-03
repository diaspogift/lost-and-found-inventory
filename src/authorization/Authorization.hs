module Authorization where 

 



import Authentication


onlyForSameId :: LocalStreamId -> IPrincipal -> (LocalStreamId -> a) 
onlyForSameId = undefined