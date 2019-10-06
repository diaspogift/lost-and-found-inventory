module DomainCommonTypes where


newtype Identifier = 
    Identifier String deriving (Eq, Ord, Show)

newtype CategoryIdentifier = 
    CategoryIdentifier String deriving (Eq, Ord, Show)

newtype Name = 
    Name String deriving (Eq, Ord, Show)



newtype CategoryCode =
    CategoryCode String deriving (Eq, Ord, Show)


newtype CategoryName =
    CategoryName String deriving (Eq, Ord, Show)


newtype CategoryDescription =
    CategoryDescription String deriving (Eq, Ord, Show)


newtype City = 
    City String deriving (Eq, Ord, Show)

newtype Village = 
    Village String deriving (Eq, Ord, Show)

newtype Neighborhood = 
    Neighborhood String deriving (Eq, Ord, Show)

newtype Address = 
     Address String deriving (Eq, Ord, Show)


newtype Description = 
    Description String deriving (Eq, Ord, Show)




newtype FirstName = 
    FirstName String deriving (Eq, Ord, Show)


newtype Middle = 
    Middle String deriving (Eq, Ord, Show)


newtype LastName = 
    LastName String deriving (Eq, Ord, Show)


newtype EmailAddress = 
    EmailAddress String deriving (Eq, Ord, Show)
newtype PostalAddress = 
    PostalAddress String deriving (Eq, Ord, Show)
newtype Telephone = 
    Telephone String deriving (Eq, Ord, Show)





newtype AttributeCode = 
    AttributeCode String deriving (Eq, Ord, Show)
newtype AttributeName = 
    AttributeName String deriving (Eq, Ord, Show)
newtype AttributeValue = 
    AttributeValue String deriving (Eq, Ord, Show)
newtype AttributeUnit = 
    AttributeUnit String deriving (Eq, Ord, Show)


newtype Question = 
    Question String deriving (Eq, Ord, Show)
 
newtype Answer = 
    Answer String deriving (Eq, Ord, Show)



