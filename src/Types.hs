module Types where


import qualified Data.Map as M


regLostItem1 :: RegisteredLostItem
regLostItem1 = RegisteredLostItem {
      liIdentifier = Identifier "regLostItem1"
    , liName = Name "Car keys"
    , liCategoryId = CategoryIdentifier (Identifier "category1")  PersonalItems
    , lostLocation = Location { 
           region = Adamaoua
        ,  department = Department (Djerem "Djerem") Adamaoua
        ,  subdivision = SubDivision {
               subDivisionCode = Gouandal "Gouandal" 
            ,  departementCode = Department (Djerem "Djerem") Adamaoua
            ,  regionCode = Adamaoua
           }
        ,  city = City "Ngaoundere"
        ,  neighborhood = Neighborhood "Chefferie"
        ,  address = Address "Rond point 3 morts"
        }
    , liDescription = 
        Description "Mes cles sont attachees a un ruban rouge et la marque de ma voiture c'est toyota"
    , liLostDate = "21/01/1985"
    , liAttributes = [Color "red", Brand "Toyota", Size  "smal"] 
}



regLostItem2 :: RegisteredLostItem
regLostItem2 = RegisteredLostItem {
      liIdentifier = Identifier "regLostItem2"
    , liName = Name "National Identity Card"
    , liCategoryId = CategoryIdentifier (Identifier "category2") PersonalItems
    , lostLocation = Location { 
           region = Adamaoua
        ,  department = Department (Djerem "Djerem") Adamaoua
        ,  subdivision = SubDivision {
               subDivisionCode = Gouandal "Gouandal" 
            ,  departementCode = Department (Djerem "Djerem") Adamaoua
            ,  regionCode = Adamaoua
           }
        ,  city = City "Ngaoundere"
        ,  neighborhood = Neighborhood "Chefferie"
        ,  address = Address "Rond point 3 morts"
        }
    , liDescription = 
        Description "Mes cles sont attaches a un ruban rouge et la marque de ma voiture c'est toyota"
    , liLostDate = "22/01/1985"
    , liAttributes = [] 
}





regLostItems = M.fromList [
    (liIdentifier regLostItem1, regLostItem1), 
    (liIdentifier regLostItem2, regLostItem2)
    ]


data Item = 
      Lost RegisteredLostItem
    | Found DeclaredFoundItem
    | Claimed ClaimedFoundItem





data RegisteredLostItem = RegisteredLostItem {
        liIdentifier :: Identifier
    ,   liName :: Name
    ,   liCategoryId :: CategoryIdentifier
    ,   lostLocation :: Location
    ,   liDescription :: Description
    ,   liLostDate :: String
    ,   liAttributes :: [Attribute]
    } deriving (Show)


data DeclaredFoundItem = DeclaredFoundItem {
        fiIdentifier :: Identifier
    ,   fiName :: Name
    ,   fiCategoryId :: CategoryIdentifier
    ,   foundLocation :: Location
    ,   fiDescription :: Description
    ,   fiFoundDate :: String
    ,   fiAttributes :: [Attribute]
    } deriving Show

data ClaimedFoundItem = ClaimedFoundItem {
        clIdentifier :: Identifier
    ,   clName :: Name
    ,   clCategoryId :: CategoryIdentifier
    ,   clfoundLocation :: Location
    ,   clDescription :: Description
    ,   clFoundDate :: String
    ,   clAttributes :: [Attribute]

    ,   claimedDate :: String
    } deriving Show   


newtype Identifier = 
    Identifier String deriving (Eq, Ord, Show)

newtype Name = 
    Name String deriving Show


data Category = Category {
        categoryIdentifier :: CategoryIdentifier
    ,   categoryDetails :: CategoryDetails
    ,   subCategories :: [Category]   
} deriving Show


data CategoryIdentifier = 
    CategoryIdentifier Identifier CategoryType deriving Show


data CategoryType = 
      Humans 
    | Documents 
    | Electronics 
    | PersonalItems
    deriving Show

data CategoryDetails = CategoryDetails {
        categoryCode :: CategoryCode
    ,   categoryName :: CategoryName
    ,   categoryDescription :: CategoryDescription
} deriving Show

newtype CategoryCode =
    CategoryCode String deriving Show

newtype CategoryName =
    CategoryName String deriving Show

newtype CategoryDescription =
    CategoryDescription String deriving Show



data Location = Location {
        region :: Region
    ,   department :: Department
    ,   subdivision :: SubDivision
    ,   city :: City
    ,   neighborhood :: Neighborhood
    ,   address :: Address
} deriving Show


data Region 
    = North
    | South
    | West
    | East
    | Center
    | NorthWest
    | SouthWest
    | ExtremeNorth
    | Adamaoua
    | Littoral
    deriving Show


data Department = 
    Department DepartmentCode Region deriving Show


data DepartmentCode 
    = Djerem String
    | FaroEtDeo String
    | MayoBanyo String
    | Mbere String
    | Vina String
    deriving Show

    



data SubDivision = SubDivision {
        subDivisionCode :: SubDivisionCode 
    ,   departementCode :: Department
    ,   regionCode :: Region
} deriving Show


data SubDivisionCode 
    = Gouandal String
    | Tibatie String
    deriving Show


newtype City = 
    City String deriving Show

newtype Neighborhood = 
    Neighborhood String deriving Show

newtype Address = 
     Address String deriving Show


newtype Description = 
    Description String deriving Show



data Attribute = 
      Color String
    | Brand String
    | Model String
    | Size String
    | Height Float
    | Weight Float
    | Manufacturer Name
    | Issuer Name
    deriving Show




newtype AttributeCode = 
    AttributeCode String deriving Show
newtype AttributeName = 
    AttributeName String deriving Show
newtype AttributeValue = 
    AttributeValue String deriving Show
newtype AttributeUnit = 
    AttributeUnit String deriving Show











