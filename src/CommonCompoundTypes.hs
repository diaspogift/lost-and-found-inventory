module CommonCompoundTypes where



import CommonSimpleTypes

import Data.Set
import Data.Time
import Data.Char
import qualified Data.Map as M



data Category = Category {
        categoryId          :: CategoryId
    ,   categoryType        :: CategoryType
    ,   parentalStatus      :: ParentalStatus
    ,   categoryDesc        :: LongDescription
    ,   subCategories       :: Set Category   
    } deriving (Eq, Ord, Show)


data ParentalStatus =
      Parent
    | Sub CategoryId CategoryType
    deriving (Eq, Ord, Show)


data CategoryType = 
      Humans
    | Documents
    | Electronics 
    | PersonalItems
    deriving (Eq, Ord, Show)

-- /// Etudier l'éventualité d'un module support independant pour les données de localisations
data Location = Location {
        region :: Region
    ,   division :: Division
    ,   subdivision :: SubDivision
    ,   city :: City
    ,   village :: Village
    ,   neighborhood :: Neighborhood
    ,   locationAddress :: Address
    } deriving (Eq, Ord, Show)


data Region 
    = Adamaoua RegionName
    | Centre RegionName
    | East RegionName
    | FarNorth RegionName
    | Littoral RegionName
    | North RegionName
    | NorthWest RegionName
    | South RegionName
    | SouthWest RegionName
    | West RegionName
    deriving (Eq, Ord, Show)


data Division
    --    Adamaoua 
        = Djerem DivisionName
        | FaroEtDeo DivisionName
        | MayoBanyo DivisionName
        | Mbere DivisionName
        | Vina DivisionName
    --    Centre
        | HauteSanaga DivisionName
        | Lekie DivisionName
        | MbamEtInoubou DivisionName
        | MbamEtKim DivisionName
        | MefouEtAfamba DivisionName
        | MefouEtAkono DivisionName
        | Mfoundi DivisionName
        | NyongEtKelle DivisionName
        | NyongEtMfoumou DivisionName
        | NyongEtSoo DivisionName
    --    Est
        | BoumbaEtNgoko DivisionName
        | HautNyong DivisionName
        | Kadey DivisionName
        | LomEtDjerem DivisionName
    --    Far North
        | Diamare DivisionName
        | LogoneEtChari DivisionName
        | MayoDanay DivisionName
        | MayoKani DivisionName
        | MayoSava DivisionName
        | MayoTsanaga DivisionName
    --    Littoral
        | Moungo DivisionName
        | Nkam DivisionName
        | SanagaMaritime DivisionName
        | Wouri DivisionName
    --    North

        | Benoue DivisionName
        | Faro DivisionName
        | MayoLouti DivisionName
        | MayoRey DivisionName
    --    NorthWest
        | Boyo DivisionName
        | Bui DivisionName
        | DongaMantung DivisionName
        | Menchum DivisionName
        | Mezam DivisionName
        | Momo DivisionName
        | Ngoketunjia DivisionName
    --    South
        | DjaEtLobo DivisionName
        | Mvila DivisionName
        | Ocean DivisionName
        | ValleeDuNtem DivisionName
    --    SouthWest
        | Fako DivisionName
        | KoupeManengouba DivisionName
        | Lebialem DivisionName
        | Manyu DivisionName
        | Meme DivisionName
        | Ndian DivisionName
    --    West
        | Bamboutos DivisionName
        | HautNkam DivisionName
        | HautsPlateaux DivisionName
        | KoungKhi DivisionName
        | Menoua DivisionName
        | Mifi DivisionName
        | Nde DivisionName
        | Noun DivisionName
        deriving (Eq, Ord, Show)

    
data SubDivision
    -- ADAMAOUA --
    --  Djerem
        = Gouandal SubDivisionName
        | Tibati SubDivisionName
    --  Faro et Deo 
        | GalimTignere SubDivisionName
        | MayoBaleo SubDivisionName
        | Tignere SubDivisionName
    --  Mayo Banyo
        | Bankim SubDivisionName
        | Banyo SubDivisionName
        | MayoDarl SubDivisionName
    -- Mbere
        | Dir SubDivisionName
        | Djohong SubDivisionName
        | Meiganga SubDivisionName
        | Ngaoui SubDivisionName
    -- Vina 
        | Belel SubDivisionName
        | Mbe SubDivisionName
        | Nganha SubDivisionName
        | NgaoundereIer SubDivisionName
        | NgaoundereIIe SubDivisionName
        | NgaoundereIIIe SubDivisionName
        | Nyambaka SubDivisionName
        | Martap SubDivisionName
    -- CENTRE --
    -- Haute Sanaga
        | Bibey SubDivisionName
        | LembeYezoum SubDivisionName
        | Mbandjock SubDivisionName
        | Minta SubDivisionName
    -- Lekie
        | Batchenga SubDivisionName
        | Ebebda SubDivisionName
        | EligMfomo SubDivisionName
        | Evodoula SubDivisionName
        | Lobo SubDivisionName
        | Monatele SubDivisionName
        | Obala SubDivisionName
        | Okola SubDivisionName
        | Saa SubDivisionName
    -- Mbam Et Inoubou
        | Bafia SubDivisionName
        | Bokito SubDivisionName
        | Deuk SubDivisionName
        | Kiiki SubDivisionName
        | KonYambetta SubDivisionName
        | Makenene SubDivisionName
        | Ndikiniméki SubDivisionName
        | Nitoukou SubDivisionName
        | Ombessa SubDivisionName
    -- Mbam et Kim

        | Mbangassina SubDivisionName
        | NgambeTikar SubDivisionName
        | Ngoro SubDivisionName
        | Ntui SubDivisionName
        | Yoko SubDivisionName
    -- MefouEtAfamba 

        | Afanloum SubDivisionName
        | AssambaOrOlanguina SubDivisionName
        | Awae SubDivisionName
        | Edzendouan SubDivisionName
        | Esse SubDivisionName
        | Mfou SubDivisionName
        | Nkolafamba SubDivisionName
        | Soa SubDivisionName
    -- MefouEtAkono 
        | Akono SubDivisionName
        | Bikok SubDivisionName
        | Mbankomo SubDivisionName
        | Ngoumou SubDivisionName
    -- Mfoundi 
        | YaoundeIOrNlongkakOrEtoudi SubDivisionName
        | YaoundeIIOrTsinga SubDivisionName
        | YaoundeIIIOrEfoulan SubDivisionName
        | YaoundeOrIVKondengui SubDivisionName
        | YaoundeVOrEssos SubDivisionName
        | YaoundeVIOrBiyemassi SubDivisionName
        | YaoundeVIIOrNkolbisson SubDivisionName
    -- NyongEtKelle 
        | Biyouha SubDivisionName
        | Bondjock SubDivisionName
        | BotMakak SubDivisionName
        | Dibang SubDivisionName
        | Eseka SubDivisionName
        | Makak SubDivisionName
        | Matomb SubDivisionName
        | Messondo SubDivisionName
        | NgogMapubi SubDivisionName
        | Nguibassal SubDivisionName
    -- NyongEtMfoumou 


        | Akonolinga SubDivisionName
        | Ayos SubDivisionName
        | Endom SubDivisionName
        | Mengang SubDivisionName
        | NyakokomboOrKobdombo SubDivisionName
    -- NyongEtSoo 


        | Akoeman SubDivisionName
        | Dzeng SubDivisionName
        | Mbalmayo SubDivisionName
        | Mengueme SubDivisionName
        | Ngomedzap SubDivisionName
        | Nkolmetet SubDivisionName
    -- EST --
    -- BoumbaEtNgoko 
        | GariGombo SubDivisionName
        | Moloundou SubDivisionName
        | Salapoumbe SubDivisionName
        | Yokadouma SubDivisionName
    -- HautNyong
        | AbongMbang SubDivisionName
        | BebendOrAtok SubDivisionName
        | Dimako SubDivisionName
        | Doumaintang SubDivisionName
        | Doume SubDivisionName
        | Lomie SubDivisionName
        | MboanzOrAngossas SubDivisionName
        | Mboma SubDivisionName
        | Messamena SubDivisionName
        | Messok SubDivisionName
        | Mindourou SubDivisionName
        | Ngoyla SubDivisionName
        | Nguelemendouka SubDivisionName
        | Somalomo SubDivisionName
    -- Kadey 

        | Batouri SubDivisionName
        | Kentzou SubDivisionName
        | Kette SubDivisionName
        | Mbang SubDivisionName
        | Ndelele SubDivisionName
        | Nguelebok SubDivisionName
        | Ouli SubDivisionName
    -- LomEtDjerem 

        | Belabo SubDivisionName
        | BertouaIer SubDivisionName
        | BertouaIIe SubDivisionName
        | BétareOya SubDivisionName
        | Diang SubDivisionName
        | GarouaBoulai SubDivisionName
        | Mandjou SubDivisionName
        | Ngoura SubDivisionName
    -- FAR NORTH --

    -- Diamare 
        | Bogo SubDivisionName
        | Dargala SubDivisionName
        | Gazawa SubDivisionName
        | MarouaIer SubDivisionName
        | MarouaIIe SubDivisionName
        | MarouaIIIe SubDivisionName
        | Meri SubDivisionName
        | Ndoukoula SubDivisionName
        | Pette SubDivisionName
    -- LogoneEtChari 

        | Blangoua SubDivisionName
        | Darak SubDivisionName
        | Fotokol SubDivisionName
        | Goulfey SubDivisionName
        | HileAlifa SubDivisionName
        | Kousseri SubDivisionName
        | LogoneBirni SubDivisionName
        | Makary SubDivisionName
        | Waza SubDivisionName
        | Zina SubDivisionName
    -- MayoDanay 
        | Datcheka SubDivisionName
        | Gobo SubDivisionName
        | Gueme SubDivisionName
        | Guere SubDivisionName
        | KaiKai SubDivisionName
        | Kalfou SubDivisionName
        | KarHay SubDivisionName
        | Maga SubDivisionName
        | TchatiBali SubDivisionName
        | Wina SubDivisionName
        | Yagoua SubDivisionName
    -- MayoKani 
        | Dziguilao SubDivisionName -- ??? Commune ou arrondiseement ???
        | Taibong SubDivisionName
        | Guidiguis SubDivisionName
        | Kaele SubDivisionName
        | Mindif SubDivisionName
        | Moulvoudaye SubDivisionName
        | Moutourwa SubDivisionName
        | Touloum SubDivisionName
    -- MayoSava
        | Kolofata SubDivisionName
        | Mora SubDivisionName
        | Tokombere SubDivisionName
    -- MayoTsanaga 
        | Bourrha SubDivisionName
        | Hina SubDivisionName
        | Koza SubDivisionName
        | Mogode SubDivisionName
        | Mokolo SubDivisionName
        | Mozogo SubDivisionName
        | SouledeRoua SubDivisionName
    -- LITTORAL --
    -- Moungo 
        | BareBakem SubDivisionName
        | Bonalea SubDivisionName
        | Dibombari SubDivisionName
        | Loum SubDivisionName
        | Manjo SubDivisionName
        | Mbanga SubDivisionName
        | Melong SubDivisionName
        | Mombo SubDivisionName
        | NjombePenjaOrPenja SubDivisionName
        | NkongsambaIer SubDivisionName
        | NkongsambaIIe SubDivisionName
        | NkongsambaIIIe SubDivisionName
        | NlonakoOrEbone SubDivisionName
    -- Nkam 
        | Nkondjock SubDivisionName
        | NordMakombeOrNdobian SubDivisionName
        | Yabassi SubDivisionName
        | Yingui SubDivisionName
    -- SanagaMaritime 
        | Dibamba SubDivisionName
        | Dizangue SubDivisionName
        | EdeaIer SubDivisionName
        | EdeaIIe SubDivisionName
        | MassockSongloulou SubDivisionName
        | Mouanko SubDivisionName
        | Ndom SubDivisionName
        | Ngambe SubDivisionName
        | Ngwei SubDivisionName
        | Nyanon SubDivisionName
        | Pouma SubDivisionName
    -- Wouri 

        | DoualaIer SubDivisionName
        | DoualaIIe SubDivisionName
        | DoualaIIIe SubDivisionName
        | DoualaIVe SubDivisionName
        | DoualaVe SubDivisionName
        | DoualaVIe SubDivisionName
        | Manoka SubDivisionName
    -- NORTH --
    -- Benoue 
        | Bibemi SubDivisionName
        | Dembo SubDivisionName
        | GarouaIer SubDivisionName
        | GarouaIIe SubDivisionName
        | GarouaIIIe SubDivisionName
        | Lagdo SubDivisionName
        | MayoHourna SubDivisionName
        | Pitoa SubDivisionName
        | Tcheboa SubDivisionName
        | Demsa SubDivisionName
        | Bascheo SubDivisionName
    -- Faro 
        | Beka SubDivisionName
        | Poli SubDivisionName
    -- MayoLouti 
        | Figuil SubDivisionName
        | Guider SubDivisionName
        | MayoOulo SubDivisionName
    -- MayoRey 
        | Madingring SubDivisionName
        | ReyBouba SubDivisionName
        | Tchollire SubDivisionName
        | Touboro SubDivisionName
    -- NORTH WEST -- 
    -- Boyo

        | Belo SubDivisionName
        | Fonfuka SubDivisionName
        | Fundong SubDivisionName
        | Njinikom SubDivisionName
    -- Bui 
        | Jakiri SubDivisionName
        | Kumbo SubDivisionName
        | MbvenOrMbiame SubDivisionName
        | Nkum SubDivisionName
        | NoniOrNkor SubDivisionName
        | OkuOrElakOku SubDivisionName
    -- DongaMantung 
        | Ako SubDivisionName
        | Misaje SubDivisionName
        | Ndu SubDivisionName
        | Nkambe SubDivisionName
        | Nwa SubDivisionName
    -- Menchum 
        | Benakuma SubDivisionName
        | FuruAwa SubDivisionName
        | Wum SubDivisionName
        | Zhoa SubDivisionName
    -- Mezam 
        | Bafut SubDivisionName
        | Bali SubDivisionName
        | BamendaIer SubDivisionName
        | BamendaIIe SubDivisionName
        | BamendaIIIe SubDivisionName
        | Santa SubDivisionName
        | Tubah SubDivisionName
    -- Momo 
        | Andek SubDivisionName
        | Batibo SubDivisionName
        | Mbengwi SubDivisionName
        | Njikwa SubDivisionName
        | WidikumBoffe SubDivisionName
    -- Ngoketunjia

        | Babessi SubDivisionName
        | Balikumbat SubDivisionName
        | Ndop SubDivisionName
    -- SOUTH 
    -- DjaEtLobo 
        | Bengbis SubDivisionName
        | Djoum SubDivisionName
        | Meyomessala SubDivisionName
        | Meyomessi SubDivisionName
        | Mintom SubDivisionName
        | Oveng SubDivisionName
        | Sangmelima SubDivisionName
        | Zoetele SubDivisionName
    -- Mvila
        | BiwongBane SubDivisionName
        | BiwongBulu SubDivisionName
        | EbolowaIer SubDivisionName
        | EbolowaIIe SubDivisionName
        | Efoulan SubDivisionName
        | Mengong SubDivisionName
        | Mvangan SubDivisionName
        | Ngoulemakong SubDivisionName
    -- Ocean

        | AkomII SubDivisionName
        | Bipindi SubDivisionName
        | Campo SubDivisionName
        | KribiIer SubDivisionName
        | KribiIIe SubDivisionName
        | Lokoundje SubDivisionName
        | Lolodorf SubDivisionName
        | Mvengue SubDivisionName
        | Niete SubDivisionName
    -- ValleeDuNtem
        | Ambam SubDivisionName
        | KyeOssi SubDivisionName
        | Maan SubDivisionName
        | Olamze SubDivisionName
    -- SOUTH WEST --
    -- Fako
        | Buea SubDivisionName
        | LimbeIer SubDivisionName
        | LimbeIIe SubDivisionName
        | LimbeIIIe SubDivisionName
        | Muyuka SubDivisionName
        | Tiko SubDivisionName
        | WestCoast SubDivisionName
    -- KoupeManengouba
        | Bangem SubDivisionName
        | Nguti SubDivisionName
        | Tombel SubDivisionName
    -- Lebialem 
        | Alou SubDivisionName
        | Fontem SubDivisionName
        | Wabane SubDivisionName
    -- Manyu
        | Akwaya SubDivisionName
        | Eyumodjock SubDivisionName
        | Mamfe SubDivisionName
        | TintoOrUpperBayang SubDivisionName
    -- Meme 
        | Konye SubDivisionName
        | KumbaIer SubDivisionName
        | KumbaIIe SubDivisionName
        | KumbaIIIe SubDivisionName
        | Mbonge SubDivisionName
    -- Ndian
        | Bamusso SubDivisionName
        | EkondoTiti SubDivisionName
        | Idabato SubDivisionName
        | Isanguele SubDivisionName
        | KomboAbedimo SubDivisionName
        | KomboItindi SubDivisionName
        | Mundemba SubDivisionName
    -- WEST -- 
    -- Bamboutos
        | Babadjou SubDivisionName
        | Batcham SubDivisionName
        | Galim SubDivisionName
        | Mbouda SubDivisionName
    -- Haut Nkam 
        | Bafang SubDivisionName
        | Bakou SubDivisionName
        | Bana SubDivisionName
        | Bandja SubDivisionName
        | Banka SubDivisionName
        | Kekem SubDivisionName
    -- Hauts Plateaux
        | Baham SubDivisionName
        | Bamendjou SubDivisionName
        | Bangou SubDivisionName
        | Batie SubDivisionName
        | Bangam SubDivisionName
        | Badenkop SubDivisionName
    -- KoungKhi
        | Bayangam SubDivisionName
        | Demdeng SubDivisionName
        | Poumougne SubDivisionName
    -- Menoua 
        | Dschang SubDivisionName
        | Fokoue SubDivisionName
        | FongoTongo SubDivisionName
        | NkongZem SubDivisionName
        | PenkaMichel SubDivisionName
        | Santchou SubDivisionName
    -- Mifi

        | BafoussamIer SubDivisionName
        | BafoussamIIe SubDivisionName
        | BafoussamIIIe SubDivisionName
    -- Nde
        | Bangangte SubDivisionName
        | Bassamba SubDivisionName
        | Bazou SubDivisionName
        | Tonga SubDivisionName
    -- Noun 
        | Bangourain SubDivisionName
        | Foumban SubDivisionName
        | Foumbot SubDivisionName
        | Kouoptamo SubDivisionName
        | Koutaba SubDivisionName
        | Magba SubDivisionName
        | Malentouen SubDivisionName
        | Massangam SubDivisionName
        | Njimom SubDivisionName
        deriving (Eq, Ord, Show)


data Attribute = Attribute {
      attrCode             :: AttributeCode
    , attrName             :: AttributeName
    , attrDescription      :: ShortDescription
    , attrValue            :: Maybe AttributeValue
    , attrUnit             :: Maybe AttributeUnit
    , relatedCategory      :: CategoryId
    , relatedCategoryType  :: CategoryType
    } deriving (Eq, Ord, Show)


data Person = Person {
    -- Revoir si user est optionelle
      userId   :: UserId
    , contact  :: ContactInformation
    , name     :: FullName
    } deriving (Eq, Ord, Show)


data ContactInformation = ContactInformation {
      -- Tel required, email optional
      email         :: EmailAddress
    , address       :: PostalAddress
    , primaryTel    :: Telephone
    , secondaryTel  :: Telephone
    } deriving (Eq, Ord, Show)


data FullName = FullName {
      first     :: FirstName
    , middle    :: Maybe Middle
    , last      :: LastName
    } deriving (Eq, Ord, Show)


data Tenant = Tenant {
      tenantId              :: TenantId
    , tenantName            ::  TenantName
    , tenantDescription     :: LongDescription
    , tenantContactAddress  :: ContactInformation
    } deriving (Eq, Show, Ord)


data Challenge = 
    Question deriving (Eq, Show, Ord) 


data ChallengeAnser = 
    ChallengeAnser (Question, Answer) deriving (Eq, Show, Ord)



--- Helper functions
---
---


--- Location helper functions
toRegion :: String -> Either ErrorMessage Region
toRegion str  
    | "adamaoua" == lowerStr = Right $ Adamaoua str
    | "centre" == lowerStr = Right $ Centre str
    | "est" == lowerStr = Right $ East str
    | "farnorth" == lowerStr = Right $ FarNorth str
    | "littoral" == lowerStr = Right $ Littoral str
    | "north" == lowerStr = Right $ North str
    | "northwest" == lowerStr = Right $ NorthWest str
    | "south" == lowerStr = Right $ South str
    | "southwest" == lowerStr = Right $ SouthWest str
    | "west" == lowerStr = Right $ West str
    | otherwise  = Left $ str <> ": is an invalid region code"
    where lowerStr = fmap toLower str

fromRegion :: Region -> String
fromRegion region = 
    case region of
        Adamaoua str -> str
        Centre str -> str
        East str -> str
        FarNorth str-> str
        Littoral str -> str
        North str -> str
        NorthWest str -> str
        South str -> str
        SouthWest str -> str
        West str-> str
   

toDivision :: String -> Either ErrorMessage Division
toDivision str
    | "faroetdeo" == lowerStr = Right $ FaroEtDeo str
    | "mayobanyo" == lowerStr = Right $ MayoBanyo str
    | "mbere" == lowerStr = Right $ Mbere str
    | "vina" == lowerStr = Right $ Vina str
    | otherwise = Left $ str <> ": is an invalid division code"
    -- TODO FINISH  ALL CASES
    where lowerStr = fmap toLower str


fromDivision :: Division -> String
fromDivision division = 
    case division of
        FaroEtDeo str -> str
        MayoBanyo str -> str
        Mbere str -> str
        Vina str -> str
        _ -> error "NOT IMPLEMENTED YET"


toSubDivision :: String -> Either ErrorMessage SubDivision
toSubDivision str  
    | "gouandal" == lowerStr = Right $ Gouandal str
    | "tibati" == lowerStr = Right $ Tibati str
    | otherwise  = Left $ str <> ": is an invalid sub division code"
    -- TODO FINISH  ALL CASES
    where lowerStr = fmap toLower str
   

fromSubDivision :: SubDivision -> String
fromSubDivision subdivision = 
    case subdivision of
        Gouandal str -> str
        Tibati str-> str
        _ -> error "NOT IMPLEMENTED YET"


--- /// Category helper functions
toCategoryType :: String -> Either ErrorMessage CategoryType
toCategoryType str
    | "humans" == lowerStr = Right Humans
    | "documents" == lowerStr = Right Documents
    | "electronics" == lowerStr = Right Electronics
    | "personalitems" == lowerStr = Right PersonalItems
    | otherwise  = Left $ str <> ": is an invalid sub division code"
    where lowerStr = fmap toLower str


fromCategoryType :: CategoryType -> String
fromCategoryType cat = 
    case cat of 
        Humans -> "Humans"
        Documents -> "Documents"
        Electronics -> "Electronics"
        PersonalItems -> "PersonalItems"


--- /// Admin Map Data Structure
data AdministrativeMap = 
    AdministrativeMap [RegionItem]  deriving Show
data RegionItem = 
    RegionItem Region [DivisionItem] deriving Show
data DivisionItem = 
    DivisionItem Division [SubDivision] deriving Show


camerounAdministrativeMap :: AdministrativeMap
camerounAdministrativeMap = AdministrativeMap allTenRegions
    where allTenRegions =
            [ adamaouaRegionItem
            ]

--- Adamaoua region
---
---

adamaouaRegionItem :: RegionItem
adamaouaRegionItem = RegionItem (Adamaoua "Adamaoua") adamaouaDivisions
    where adamaouaDivisions = 
            [ djeremDivision
            , faroEtDeoDivision
            , mayoBanyoDivision
            , mbereDivision
            , vinaDivision
            ]

djeremDivision :: DivisionItem
djeremDivision = DivisionItem (Djerem "Djerem") djeremSubDivisions
    where djeremSubDivisions =
            [  (Gouandal "Gouandal")
            ,  (Tibati "Tibati")
            ]

faroEtDeoDivision :: DivisionItem
faroEtDeoDivision = DivisionItem (FaroEtDeo "Faro et Deo") faroEtDeroSubDivisions
    where faroEtDeroSubDivisions =
            [   GalimTignere "GalimTignere"
            ,   MayoBaleo "Mayo Baléo"
            ,   Tignere "Tignere"
            ]

mayoBanyoDivision :: DivisionItem
mayoBanyoDivision = DivisionItem (MayoBanyo "Mayo Banyo") mayoBanyoSubDivisions
    where mayoBanyoSubDivisions =
            [  Bankim "Bankim"    
            ,  Banyo "Banyo"
            ,  MayoDarl "MayoDarl"
            ]

mbereDivision :: DivisionItem
mbereDivision = DivisionItem (Mbere "Mbéré") mbereSubDivisions
    where mbereSubDivisions =
            [  Dir "Dir"
            ,  Djohong "Djohong"
            ,  Meiganga "Meiganga"
            ,  Ngaoui "Ngaoui"
            ]

vinaDivision :: DivisionItem
vinaDivision = DivisionItem (Vina "Vina") vinaSubDivisions
    where vinaSubDivisions =
            [ Belel "Belel"
            , Mbe "Mbe"  
            , Nganha "Nganha"
            , NgaoundereIer "Ngaoundere Ier"
            , NgaoundereIIe "Ngaoundere IIe"
            , NgaoundereIIIe "Ngaoundere IIIe"
            , Nyambaka "Nyambaka"
            , Martap "Martap" 
            ]



{--
--- Centre region
---
---

centreRegionItem :: RegionItem
centreRegionItem = RegionItem Centre centreDivisions
    where centreDivisions = 
            [ hauteSanagaDivision
            , lekieDivision
            , mbamEtInoubouDivision
            , mbamEtKimDivision
            , mefouEtAfambaDivision
            , mefouEtAkonoDivision
            , mfoundiDivision
            , nyongEtKelleDivision
            , nyongEtMfoumouDivision
            , nyongEtSooDivision
            ]

          

hauteSanagaDivision :: DivisionItem
hauteSanagaDivision = DivisionItem HauteSanaga hauteSanagaSubDivisions
    where hauteSanagaSubDivisions =
            [ Bibey
            , LembeYezoum
            , Mbandjock
            , Minta
            ]

lekieDivision :: DivisionItem
lekieDivision = DivisionItem Lekie lekieSubDivisions
    where lekieSubDivisions =
            [ Batchenga
            , Ebebda
            , EligMfomo
            , Evodoula
            , Lobo
            , Monatélé
            , Obala
            , Okola
            , Saa    
            ]

mbamEtInoubouDivision :: DivisionItem
mbamEtInoubouDivision = DivisionItem MbamEtInoubou mbamEtInoubouSubDivisions
    where mbamEtInoubouSubDivisions =
            [
                
            ]

mbamEtKimDivision :: DivisionItem
mbamEtKimDivision = DivisionItem MbamEtKim mbamEtKimSubDivisions
    where mbamEtKimSubDivisions =
            [
                
            ]

mefouEtAfambaDivision :: DivisionItem
mefouEtAfambaDivision = DivisionItem MefouEtAfamba mefouEtAfambaSubDivisions
    where mefouEtAfambaSubDivisions =
            [
                
            ]

mefouEtAkonoDivision :: DivisionItem
mefouEtAkonoDivision = DivisionItem MefouEtAkono mefouEtAkonoSubDivisions
    where mefouEtAkonoSubDivisions =
            [
                
            ]

mfoundiDivision :: DivisionItem
mfoundiDivision = DivisionItem Mfoundi mfoundiSubDivisions
    where mfoundiSubDivisions =
            [
                
            ]

nyongEtKelleDivision :: DivisionItem
nyongEtKelleDivision = DivisionItem NyongEtKelle nyongEtKelleSubDivisions
    where nyongEtKelleSubDivisions =
            [
                
            ]

nyongEtMfoumouDivision :: DivisionItem
nyongEtMfoumouDivision = DivisionItem NyongEtMfoumou nyongEtMfoumouSubDivisions
    where nyongEtMfoumouSubDivisions =
            [
                
            ]


nyongEtSooDivision :: DivisionItem
nyongEtSooDivision = DivisionItem NyongEtSoo nyongEtSooDivisionSubDivisions
    where nyongEtSooDivisionSubDivisions =
            [
                
            ]


--- Far North region
---
---

adamaouaRegionItem :: RegionItem
adamaouaRegionItem = RegionItem Adamaoua adamaouaDivisions
    where adamaouaDivisions = 
            [ 
 
            ]


adamaouaDjeremDivision :: DivisionItem
adamaouaDjeremDivision = DivisionItem Djerem djeremSubDivisions
    where djeremSubDivisions =
            [

            ]

adamaouaFaroEtDeoDivision :: DivisionItem
adamaouaFaroEtDeoDivision = DivisionItem FaroEtDeo faroEtDeroSubDivisions
    where faroEtDeroSubDivisions =
            [
                
            ]

adamaouaMayoBanyoDivision :: DivisionItem
adamaouaMayoBanyoDivision = DivisionItem MayoBanyo mayoBanyoSubDivisions
    where mayoBanyoSubDivisions =
            [
                
            ]

adamaouaMbereDivision :: DivisionItem
adamaouaMbereDivision = DivisionItem Mbere mbereSubDivisions
    where mbereSubDivisions =
            [
                
            ]

adamaouaVinaDivision :: DivisionItem
adamaouaVinaDivision = DivisionItem Vina vinaSubDivisions
    where vinaSubDivisions =
            [
                
            ]
--}

 




--- /// Region Names lookup
regionNamesMap :: M.Map String Region
regionNamesMap = M.fromList [
        ("adamaoua", Adamaoua "Adamaoua")
    ,   ("centre", Centre "Centre")
    ,   ("east", East "East")
    ,   ("farnorh", FarNorth "Far North")
    ,   ("littoral", Littoral "Littoral")
    ,   ("north", North "Nort")
    ,   ("northwest", NorthWest "North West")
    ,   ("south", South "South")
    ,   ("southwest", SouthWest "South West")
    ,   ("west", West "West")
    ]

--- /// Division Names lookup
divisionNamesMap :: M.Map String Division
divisionNamesMap = M.fromList [
    -- Adamaoua
        ("djerem", Djerem "Djerem")
    ,   ("faroetdeo", FaroEtDeo "Faro et Déo")
    ,   ("mayobanyo", MayoBanyo "Mayo Banyo")
    ,   ("mbere", Mbere "Mbéré")
    ,   ("vina", Vina "Vina")
    -- Centre
    ,   ("hautesanaga", HauteSanaga "Haute Sanaga")
    ,   ("lekie", Lekie "Lekié")
    ,   ("mbametinoubou", MbamEtInoubou "Mbam et Inoubou")
    ,   ("mbametkim", MbamEtKim "Mbam et Kim")
    ,   ("mefouetafamba", MefouEtAfamba "Mefou et Afamba")
    ,   ("mefouetakono", MefouEtAkono "Mefou et Akono")
    ,   ("mfoundi", Mfoundi "Mfoundi")
    ,   ("nyongetkelle", NyongEtKelle "Nyong et Kelle")
    ,   ("nyongetmfoumou", NyongEtMfoumou "Nyong et Mfoumou")
    ,   ("nyongetsoo", NyongEtSoo "Nyong et Soo")
    -- East
    ,   ("boumbaetngoko", BoumbaEtNgoko "Boumba et Ngoko")
    ,   ("hautnyong", HautNyong "Haut Nyong")
    ,   ("kadey", Kadey "Kadey")
    ,   ("lometdjerem", LomEtDjerem "Lom et Djerem")
    -- Far North
    ,   ("diamare", Diamare "Diamare")
    ,   ("logoneetchari", LogoneEtChari "Logone et Chari")
    ,   ("mayodanay", MayoDanay "Mayo Danay")
    ,   ("mayokani", MayoKani "Mayo Kani")
    ,   ("mayosava", MayoSava "Mayo Sava")
    ,   ("mayotsanaga", MayoTsanaga "Mayo Tsanaga")
    -- Littoral
    ,   ("moungo", Moungo "Moungo")
    ,   ("mkam", Nkam "Nkam")
    ,   ("sanagamaritime", SanagaMaritime "Sanaga Maritime")
    ,   ("wouri", Wouri "Wouri")
    -- North
    ,   ("benoue", Benoue "Benoue")
    ,   ("faro", Faro "Faro")
    ,   ("mayolouti", MayoLouti "Mayo Louti")
    ,   ("mayorey", MayoRey "Mayo Rey")
    --  NorthWest
    ,   ("boyo", Boyo "Boyo")
    ,   ("bui", Bui "Bui")
    ,   ("dongamantung", DongaMantung "Donga Mantung")
    ,   ("Menchum", Menchum "Menchum")
    ,   ("mezam", Mezam "Mezam")
    ,   ("momo", Momo "Momo")
    ,   ("ngoketunjia", Ngoketunjia "Ngoketunjia")
    -- South 
    ,   ("djaetlobo", DjaEtLobo "Dja et Lobo")
    ,   ("mvila", Mvila "Mvila")
    ,   ("ocean", Ocean "Ocean")
    ,   ("valleeduntem", ValleeDuNtem "Vallée du Ntem")
    -- SouthWest
    ,   ("fako", Fako "Fako")
    ,   ("koupemanengouba", KoupeManengouba "Koupé Manengouba")
    ,   ("lebialem", Lebialem "Lebialem")
    ,   ("manyu", Manyu "Manyu")
    ,   ("meme", Meme "Meme")
    ,   ("ndian", Ndian "Ndian")
    -- West
    ,   ("bamboutos", Bamboutos "Bamboutos")
    ,   ("hautnkam", HautNkam "Haut Nkam")
    ,   ("hautsplateaux", HautsPlateaux "Hauts Plateaux")
    ,   ("koungKhi", KoungKhi "KoungKhi")
    ,   ("menoua", Menoua "Menoua")
    ,   ("mifi", Mifi "Mifi")
    ,   ("nde", Nde "Nde")
    ,   ("noun", Noun "Noun")
    ]

--- Subdivision Names lookup
subDivisionNamesMap :: M.Map String SubDivision
subDivisionNamesMap = M.fromList [
    -- Adamaoua
        -- Djerem
        ("gouandal", Gouandal "Gouandal")
    ,   ("tibati", Tibati "Tibati")
        --  Faro et Deo 
    ,   ("galimtignere", GalimTignere "GalimTignere")
    ,   ("mayobaleo", MayoBaleo "Mayo Baléo")
    ,   ("tignere", Tignere "Tignere")
        --  Mayo Banyo
    ,   ("bankim", Bankim "Bankim")    
    ,   ("banyo", Banyo "Banyo")
    ,   ("mayodarl", MayoDarl "MayoDarl")
        -- Mbere
    ,   ("dir", Dir "Dir")
    ,   ("djohong", Djohong "Djohong")
    ,   ("meiganga", Meiganga "Meiganga")
    ,   ("ngaoui", Ngaoui "Ngaoui")
        -- Vina 
    ,   ("belel", Belel "Belel")
    ,   ("mbe", Mbe "Mbe")    
    ,   ("nganha", Nganha "Nganha")
    ,   ("ngaounderepremier", NgaoundereIer "Ngaoundere Ier")
    ,   ("ngaounderedeuxieme", NgaoundereIIe "Ngaoundere IIe")
    ,   ("Ngaounderetroisieme", NgaoundereIIIe "Ngaoundere IIIe")
    ,   ("nyambaka", Nyambaka "Nyambaka")
    ,   ("martap", Martap "Martap")
    -- Centre
        -- Haute Sanaga 
    ,   ("bibey", Bibey "Bibey")
    ,   ("lembeyezoum", LembeYezoum "Lembe Yezoum")    
    ,   ("mbandjock", Mbandjock "Mbandjock")
    ,   ("minta", Minta "Minta")
        -- Lekie
    ,   ("batchenga", Batchenga "Batchenga")
    ,   ("ebebda", Ebebda "Ebebda")
    ,   ("eligmfomo", EligMfomo "Elig Mfomo")
    ,   ("evodoula", Evodoula "Evodoula")    
    ,   ("lobo", Lobo "Lobo")
    ,   ("monatele", Monatele "Monatélé")
    ,   ("obala", Obala "Obala")
    ,   ("okola", Okola "Okola") 
    ,   ("saa", Saa "Saa")
        -- Mbam Et Inoubou
    ,   ("bafia", Bafia "Bafia")
    ,   ("bokito", Bokito "Bokito")
    ,   ("deuk", Deuk "Deuk")    
    ,   ("kiiki", Kiiki "Kiiki")
    ,   ("konYambetta", KonYambetta "Kon Yambetta")
    ,   ("makenene", Makenene "Makenene")
    ,   ("ndikinimeki", Ndikiniméki "Ndikiniméki")   
    ,   ("nitoukou", Nitoukou "Nitoukou")   
    ,   ("ombessa", Ombessa "Ombessa")  
        -- Mbam et Kim
    ,   ("mbangassina", Mbangassina "Mbangassina")
    ,   ("ngambetikar", NgambeTikar "Ngambe Tikar")    
    ,   ("ngoro", Ngoro "Ngoro")
    ,   ("ntui", Ntui "Ntui")
    ,   ("yoko", Yoko "Yoko")
        -- MefouEtAfamba 
    ,   ("afanloum", Afanloum "Afanloum")
    ,   ("assambarrolanguina", AssambaOrOlanguina "Assamba / Olanguina")    
    ,   ("awae", Awae "Awae")
    ,   ("edzendouan", Edzendouan "Edzendouan")
    ,   ("esse", Esse "Esse")
    ,   ("mfou", Mfou "Mfou")
    ,   ("nkolafamba", Nkolafamba "Nkolafamba")
    ,   ("soa", Soa "Soa")
        -- MefouEtAkono 
    ,   ("akono", Akono "Akono")
    ,   ("bikok", Bikok "Bikok")    
    ,   ("mbankomo", Mbankomo "Mbankomo")
    ,   ("ngoumou", Ngoumou "Ngoumou")
        -- Mfoundi 
    ,   ("yaoundepremierornlongkakoretoudi", YaoundeIOrNlongkakOrEtoudi "Yaounde I / Nlongkak / Etoudi")
    ,   ("yaoundesecondortsinga", YaoundeIIOrTsinga "Yaounde II / Tsinga")
    ,   ("yaoundetroisiemeorefoulan", YaoundeIIIOrEfoulan "Yaounde III / Efoulan")
    ,   ("yaoundequatriemeorkondengui", YaoundeOrIVKondengui "Yaounde IV / Kondengui")
    ,   ("yaoundecinquiemeoressos", YaoundeVOrEssos "Yaounde V / Essos")
    ,   ("yaoundesixiemeorbiyemassi", YaoundeVIOrBiyemassi "Yaounde VI / BiyemAssi")
    ,   ("yaoundeseptiemeornkolbisson", YaoundeVIIOrNkolbisson "Yaounde VII / Nkolbisson")
        -- NyongEtKelle 
    ,   ("biyouha", Biyouha "Biyouha")
    ,   ("bondjock", Bondjock "Bondjock")
    ,   ("botMakak", BotMakak "BotMakak")
    ,   ("dibang", Dibang "Dibang")
    ,   ("eseka", Eseka "Eseka")
    ,   ("makak", Makak "Makak")
    ,   ("matomb", Matomb "Matomb")
    ,   ("messondo", Messondo "Messondo")
    ,   ("ngogmapubi", NgogMapubi "Ngog Mapubi")
    ,   ("nguibassal", Nguibassal "Nguibassal")
        -- NyongEtMfoumou 
    ,   ("akonolinga", Akonolinga "Akonolinga")
    ,   ("ayos", Ayos "Ayos")
    ,   ("endom", Endom "Endom")
    ,   ("mengang", Mengang "Mengang")
    ,   ("nyakokomboorkobdombo", NyakokomboOrKobdombo "Nyakokombo / Kobdombo")
        -- NyongEtSoo 
    ,   ("akoeman", Akoeman "Akoeman")
    ,   ("dzeng", Dzeng "Dzeng")
    ,   ("mbalmayo", Mbalmayo "Mbalmayo")
    ,   ("mengueme", Mengueme "Mengueme")
    ,   ("ngomedzap", Ngomedzap "Ngomedzap")
    ,   ("nkolmetet", Nkolmetet "Nkolmetet")
    -- East
        -- BoumbaEtNgoko 
    ,   ("garigombo", GariGombo "Gari Gombo")
    ,   ("moloundou", Moloundou "Moloundou")
    ,   ("salapoumbe", Salapoumbe "Salapoumbe")
    ,   ("yokadouma", Yokadouma "Yokadouma")    
        -- HautNyong
    ,   ("abongmbang", AbongMbang "Abong Mbang")
    ,   ("bebendorratok", BebendOrAtok "Bebend / Atok")
    ,   ("dimako", Dimako "Dimako")
    ,   ("doumaintang", Doumaintang "Doumaintang")
    ,   ("doume", Doume "Doume")
    ,   ("lomie", Lomie "Lomie")
    ,   ("mboanzorangossas", MboanzOrAngossas "Mboanz / Angossas")
    ,   ("mboma", Mboma "Mboma")    
    ,   ("messamena", Messamena "Messamena")
    ,   ("messok", Messok "Messok")
    ,   ("mindourou", Mindourou "Mindourou")
    ,   ("ngoyla", Ngoyla "Ngoyla")
    ,   ("nguelemendouka", Nguelemendouka "Nguelemendouka")
    ,   ("somalomo", Somalomo "Somalomo")
        -- Kadey 
    ,   ("batouri", Batouri "Batouri")
    ,   ("kentzou", Kentzou "Kentzou")
    ,   ("kette", Kette "Kette")
    ,   ("mbang", Mbang "Mbang")
    ,   ("ndelele", Ndelele "Ndelele")
    ,   ("nguelebok", Nguelebok "Nguelebok")
    ,   ("ouli", Ouli "Ouli")
        -- LomEtDjerem 
    ,   ("belabo", Belabo "Belabo")
    ,   ("bertouapremier", BertouaIer "Bertoua Ier")
    ,   ("bertouadeuxieme", BertouaIIe "Bertoua IIe")
    ,   ("betareoya", BétareOya "Bétare Oya")
    ,   ("diang", Diang "Diang")
    ,   ("garouaboulai", GarouaBoulai "Garoua Boulai")
    ,   ("ngoura", Ngoura "Ngoura")
    -- Far North
        -- Diamare 
    ,   ("bogo", Bogo "Bogo")
    ,   ("dargala", Dargala "Dargala")
    ,   ("gazawa", Gazawa "Gazawa")
    ,   ("marouapremier", MarouaIer "Maroua Ier")    
    ,   ("marouadeuxieme", MarouaIIe "Maroua IIe")
    ,   ("marouatroisieme", MarouaIIIe "Maroua IIIe")
    ,   ("meri", Meri "Meri")
    ,   ("ndoukoula", Ndoukoula "Ndoukoula")
    ,   ("pette", Pette "Pette")
        -- LogoneEtChari 
    ,   ("blangoua", Blangoua "Blangoua")
    ,   ("darak", Darak "Darak")
    ,   ("fotokol", Fotokol "Fotokol")
    ,   ("goulfey", Goulfey "Goulfey")    
    ,   ("hilealifa", HileAlifa "Hile Alifa")
    ,   ("kousseri", Kousseri "Kousseri")
    ,   ("logonebirni", LogoneBirni "Logone Birni")
    ,   ("makary", Makary "Makary")
    ,   ("waza", Waza "Waza")
    ,   ("zina", Zina "Zina")
        -- MayoDanay
    ,   ("datcheka", Datcheka "Datcheka")
    ,   ("gobo", Gobo "Gobo")    
    ,   ("gueme", Gueme "Gueme")
    ,   ("guere", Guere "Guere")
    ,   ("kaiKai", KaiKai "KaiKai")
    ,   ("kalfou", Kalfou "Kalfou")
    ,   ("karHay", KarHay "KarHay")
    ,   ("maga", Maga "Maga")    
    ,   ("tchatibali", TchatiBali "Tchati Bali")
    ,   ("wina", Wina "Wina")
    ,   ("yagoua", Yagoua "Yagoua")
        -- MayoKani 
    ,   ("dziguilao", Dziguilao "Dziguilao") -- ??? Commune ou arrondiseement ???
    ,   ("taibong", Taibong "Taibong")    
    ,   ("guidiguis", Guidiguis "Guidiguis")
    ,   ("kaele", Kaele "Kaele")
    ,   ("mindif", Mindif "Mindif")
    ,   ("moulvoudaye", Moulvoudaye "Moulvoudaye")
    ,   ("moutourwa", Moutourwa "Moutourwa")
    ,   ("touloum", Touloum "Touloum")    
        -- MayoSava
    ,   ("kolofata", Kolofata "Kolofata")
    ,   ("mora", Mora "Mora")
    ,   ("tokombere", Tokombere "Tokombere")
        -- MayoTsanaga 
    ,   ("bourrha", Bourrha "Bourrha")
    ,   ("hina", Hina "Hina")
    ,   ("koza", Koza "Koza")
    ,   ("mogode", Mogode "Mogode") 
    ,   ("mokolo", Mokolo "Mokolo")
    ,   ("mozogo", Mozogo "Mozogo")
    ,   ("soulederoua", SouledeRoua "Soulede Roua")
    -- Littoral
        -- Moungo 
    ,   ("barebakem", BareBakem "Bare Bakem")
    ,   ("bonalea", Bonalea "Bonalea")
    ,   ("dibombari", Dibombari "Dibombari")
    ,   ("loum", Loum "Loum")    
    ,   ("manjo", Manjo "Manjo")
    ,   ("mbanga", Mbanga "Mbanga")
    ,   ("melong", Melong "Melong")
    ,   ("mombo", Mombo "Mombo")
    ,   ("njombepenjaorpenja", NjombePenjaOrPenja "Njombe Penja / Penja")
    ,   ("nkongsambapremier", NkongsambaIer "Nkongsamba Ier")
    ,   ("nkongsambadeuxieme", NkongsambaIIe "Nkongsamba IIe")
    ,   ("nkongsambatroisieme", NkongsambaIIIe "Nkongsamba IIIe")    
    ,   ("nlonakoorebone", NlonakoOrEbone "Nlonako / Ebone")
        -- Nkam
    ,   ("nkondjock", Nkondjock "Nkondjock")
    ,   ("nordmakombeorndobian", NordMakombeOrNdobian "Nord Makombe / Ndobian")
    ,   ("yabassi", Yabassi "Yabassi")
    ,   ("yingui", Yingui "Yingui")
        -- SanagaMaritime
    ,   ("bibamba", Dibamba "Dibamba")
    ,   ("dizangue", Dizangue "Dizangue")
    ,   ("edeapremier", EdeaIer "Edéa Ier")
    ,   ("edeadeuxieme", EdeaIIe "Edéa IIe")
    ,   ("massocksongloulou", MassockSongloulou "Massock Songloulou") 
    ,   ("mouanko", Mouanko "Mouanko")
    ,   ("ndom", Ndom "Ndom")
    ,   ("ngambe", Ngambe "Ngambe")
    ,   ("ngwei", Ngwei "Ngwei")
    ,   ("nyanon", Nyanon "Nyanon") 
    ,   ("pouma", Pouma "Pouma")
        -- Wouri 
    ,   ("doualapremier", DoualaIer "Douala Ier")
    ,   ("doualadeuxieme", DoualaIIe "Douala IIe")
    ,   ("doualatroisieme", DoualaIIIe "Douala IIIe")
    ,   ("doualaquatrieme", DoualaIVe "Douala IVe")  
    ,   ("doualacinquieme", DoualaVe "Douala Ve")
    ,   ("doualasixiemee", DoualaVIe "Douala VIe")
    ,   ("manoka", Manoka "Manoka")
    -- North
        -- Benoue 
    ,   ("bibemi", Bibemi "Bibemi")
    ,   ("dembo", Dembo "Dembo")
    ,   ("garouapremier", GarouaIer "Garoua Ier")
    ,   ("garouadeuxieme", GarouaIIe "Garoua IIe")    
    ,   ("garouatroisieme", GarouaIIIe "Garoua IIIe")
    ,   ("lagdo", Lagdo "Lagdo")
    ,   ("mayohourna", MayoHourna "Mayo Hourna")
    ,   ("pitoa", Pitoa "Pitoa")
    ,   ("tcheboa", Tcheboa "Tcheboa")
    ,   ("demsa", Demsa "Demsa")
    ,   ("bascheo", Bascheo "Bascheo")
        -- Faro 
    ,   ("beka", Beka "Beka")
    ,   ("poli", Poli "Poli")
        -- MayoLouti
    ,   ("figuil", Figuil "Figuil")
    ,   ("guider", Guider "Guider")
    ,   ("mayooulo", MayoOulo "Mayo Oulo")
        -- Mayo Rey 
    ,   ("madingring", Madingring "Madingring")
    ,   ("reybouba", ReyBouba "Rey Bouba")
    ,   ("tchollire", Tchollire "Tchollire")
    ,   ("touboro", Touboro "Touboro")    
    --  NorthWest
        -- Boyo
    ,   ("belo", Belo "Belo")
    ,   ("fonfuka", Fonfuka "Fonfuka")
    ,   ("fundong", Fundong "Fundong")
    ,   ("njinikom", Njinikom "Njinikom")    
        -- Bui 
    ,   ("jakiri", Jakiri "Jakiri")
    ,   ("kumbo", Kumbo "Kumbo")
    ,   ("mbvenormbiame", MbvenOrMbiame "Mbven / Mbiame")
    ,   ("nkum", Nkum "Nkum")    
    ,   ("noniorkkor", NoniOrNkor "Noni / Nkor")
    ,   ("okuorelakoku", OkuOrElakOku "Oku / Elak Oku")
        -- DongaMantung 
    ,   ("ako", Ako "Ako")
    ,   ("misaje", Misaje "Misaje")
    ,   ("ndu", Ndu "Ndu")
    ,   ("nkambe", Nkambe "Nkambe")    
    ,   ("nwa", Nwa "Nwa")
        -- Menchum 
    ,   ("benakuma", Benakuma "Benakuma")
    ,   ("furuawa", FuruAwa "Furu Awa")
    ,   ("wum", Wum "Wum")
    ,   ("zhoa", Zhoa "Zhoa")    
        -- Mezam 
    ,   ("bafut", Bafut "Bafut")
    ,   ("bali", Bali "Bali")
    ,   ("bamendapremier", BamendaIer "Bamenda Ier")
    ,   ("bamendadeuxieme", BamendaIIe "Bamenda IIe")    
    ,   ("bamendatroisieme", BamendaIIIe "Bamenda IIIe")
    ,   ("santa", Santa "Santa")
    ,   ("tubah", Tubah "Tubah")
        -- Momo
    ,   ("andek", Andek "Andek")
    ,   ("batibo", Batibo "Batibo")
    ,   ("mbengwi", Mbengwi "Mbengwi")
    ,   ("njikwa", Njikwa "Njikwa")    
    ,   ("widikumboffe", WidikumBoffe "Widikum Boffe")
        -- Ngoketunjia
    ,   ("babessi", Babessi "Babessi")
    ,   ("balikumbat", Balikumbat "Balikumbat")
    ,   ("ndop", Ndop "Ndop")
    -- South 
        -- DjaEtLobo 
    ,   ("bengbis", Bengbis "Bengbis")
    ,   ("djoum", Djoum "Djoum")
    ,   ("meyomessala", Meyomessala "Meyomessala")
    ,   ("meyomessi", Meyomessi "Meyomessi")    
    ,   ("mintom", Mintom "Mintom")
    ,   ("oveng", Oveng "Oveng")
    ,   ("sangmelima", Sangmelima "Sangmelima")
    ,   ("zoetele", Zoetele "Zoetele")
        -- Mvila
    ,   ("biwongbane", BiwongBane "Biwong Bane")
    ,   ("biwongbulu", BiwongBulu "Biwong Bulu")
    ,   ("ebolowapremier", EbolowaIer "Ebolowa Ier")
    ,   ("ebolowadeuxieme", EbolowaIIe "Ebolowa IIe")    
    ,   ("efoulan", Efoulan "Efoulan")
    ,   ("mengong", Mengong "Mengong")
    ,   ("mvangan", Mvangan "Mvangan")
    ,   ("ngoulemakong", Ngoulemakong "Ngoulemakong")
        -- Ocean
    ,   ("akomdeux", AkomII "Akom II")
    ,   ("bipindi", Bipindi "Bipindi")
    ,   ("campo", Campo "Campo")
    ,   ("kribipremier", KribiIer "Kribi Ier")    
    ,   ("kribideuxieme", KribiIIe "Kribi IIe")
    ,   ("lolodorf", Lolodorf "Lolodorf")
    ,   ("mvengue", Mvengue "Mvengue")
    ,   ("niete", Niete "Niete")
        -- ValleeDuNtem
    ,   ("ambam", Ambam "Ambam")
    ,   ("kyeossi", KyeOssi "Kye Ossi")
    ,   ("maan", Maan "Maan") -- ??
    ,   ("olamze", Olamze "Olamze")    
    -- SouthWest
        -- Fako
    ,   ("buea", Buea "Buéa")
    ,   ("limbepremier", LimbeIer "Limbé Ier")
    ,   ("limbedeuxieme", LimbeIIe "Limbé IIe")
    ,   ("limbetroisieme", LimbeIIIe "Limbé IIIe")    
    ,   ("muyuka", Muyuka "Muyuka")
    ,   ("tiko", Tiko "Tiko")
    ,   ("westcoast", WestCoast "West Coast")
        -- KoupeManengouba
    ,   ("bangem", Bangem "Bangem")
    ,   ("nguti", Nguti "Nguti")
    ,   ("tombel", Tombel "Tombel")
        -- Lebialem 
    ,   ("alou", Alou "Alou")
    ,   ("fontem", Fontem "Fontem")
    ,   ("wabane", Wabane "Wabane")
        -- Manyu
    ,   ("akwaya", Akwaya "Akwaya")
    ,   ("eyumodjock", Eyumodjock "Eyumodjock")
    ,   ("mamfe", Mamfe "Mamfe")
    ,   ("tintoorupperbayang", TintoOrUpperBayang "Tinto / Upper / Bayang")    
        -- Meme 
    ,   ("konye", Konye "Konye")
    ,   ("kumbapremier", KumbaIer "Kumba Ier")
    ,   ("kumbadeuxieme", KumbaIIe "Kumba IIe")
    ,   ("kumbatroisieme", KumbaIIIe "Kumba IIIe")    
    ,   ("mbonge", Mbonge "Mbonge")
        -- Ndian
    ,   ("bamusso", Bamusso "Bamusso")
    ,   ("ekondotiti", EkondoTiti "Ekondo Titi")
    ,   ("idabato", Idabato "Idabato")
    ,   ("isanguele", Isanguele "Isanguele")    
    ,   ("komboabedimo", KomboAbedimo "Kombo Abedimo")
    ,   ("komboitindi", KomboItindi "Kombo Itindi")
    ,   ("mundemba", Mundemba "Mundemba")
    -- West
        -- Bamboutos
    ,   ("babadjou", Babadjou "Babadjou")
    ,   ("batcham", Batcham "Batcham")
    ,   ("galim", Galim "Galim")
    ,   ("mbouda", Mbouda "Mbouda")    
        -- Haut Nkam 
    ,   ("bafang", Bafang "Bafang")
    ,   ("bakou", Bakou "Bakou")
    ,   ("bana", Bana "Bana")
    ,   ("bandja", Bandja "Bandja")    
    ,   ("banka", Banka "Banka")
    ,   ("kekem", Kekem "Kekem")
        -- Hauts Plateaux
    ,   ("baham", Baham "Baham")
    ,   ("bamendjou", Bamendjou "Bamendjou")
    ,   ("bangou", Bangou "Bangou")
    ,   ("batie", Batie "Batie")    
    ,   ("bangam", Bangam "Bangam")
    ,   ("badenkop", Badenkop "Badenkop")
        -- KoungKhi
    ,   ("bayangam", Bayangam "Bayangam")
    ,   ("demdeng", Demdeng "Demdeng")
    ,   ("poumougne", Poumougne "Poumougne")
        -- Menoua 
    ,   ("dschang", Dschang "Dschang")
    ,   ("fokoue", Fokoue "Fokoué")
    ,   ("fongotongo", FongoTongo "Fongo Tongo")
    ,   ("nkongzem", NkongZem "Nkong Zem")    
    ,   ("penkamichel", PenkaMichel "Penka Michel")
    ,   ("santchou", Santchou "Santchou")
        -- Mifi
    ,   ("bafoussampremier", BafoussamIer "Bafoussam Ier")
    ,   ("bafoussamdeuxieme", BafoussamIIe "Bafoussam IIe")
    ,   ("bafoussamtroisieme", BafoussamIIIe "Bafoussam IIIe")
        -- Nde
    ,   ("bangangte", Bangangte "Bangangte")
    ,   ("bassamba", Bassamba "Bassamba")
    ,   ("bazou", Bazou "Bazou")
    ,   ("tonga", Tonga "Tonga")    
        -- Noun
    ,   ("Bangourain", Bangourain "Bangourain")
    ,   ("Foumban", Foumban "Foumban")
    ,   ("Foumbot", Foumbot "Foumbot")
    ,   ("Kouoptamo", Kouoptamo "Kouoptamo")    
    ,   ("Koutaba", Koutaba "Koutaba")
    ,   ("Magba", Magba "Magba")
    ,   ("Malentouen", Malentouen "Malentouen")
    ,   ("Massangam", Massangam "Massangam") 
    ,   ("Njimom", Njimom "Njimom")
    ]






 
