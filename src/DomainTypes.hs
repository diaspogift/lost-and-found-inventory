module DomainTypes where



import DomainCommonTypes

import qualified Data.Map as M
import Data.Set
import Data.Time
import Data.Char



data Item = 
      Lost LostItem
    | Found FoundItem
    | Matched MatchedItem
    | Claimed ClaimedItem


data LostItem = LostItem {
        lostItemId          :: LostItemId
    ,   lostItemName        :: ItemName
    ,   lostItemCategoryId  :: CategoryId
    ,   lostItemLocation    :: Location
    ,   lostItemDesc        :: LongDescription
    ,   lostItemLostDate    :: UTCTime
    ,   lostItemAttributes  :: [Attribute]
    ,   lostItemOwner       :: Person
    } deriving (Eq, Ord, Show)


data FoundItem = FoundItem {
        foundItemId         :: FoundItemId
    ,   foundItemName       :: ItemName
    ,   foundItemCategoryId :: CategoryId
    ,   foundItemLocation   :: Location
    ,   foundItemDesc       :: LongDescription
    ,   foundItemFoundDate  :: UTCTime
    ,   foundItemAttributes :: [Attribute]
    ,   foundItemDeclarant  :: Person
    ,   foundItemTenant     :: Tenant

    ,   chalenges :: [Challenge]
    } deriving (Eq, Ord, Show)


data MatchedItem = MatchedItem {
        matchedItemId       :: MatchedItemId
    ,   matchedFromLostItem :: LostItem
    ,   declaredFoundItems  :: Set FoundItem
    } deriving (Eq, Ord, Show)  


data ClaimedItem = ClaimedItem {
        claimedItemId       :: ClaimedItemId
    ,   claimedFromLostItem :: LostItem
    ,   claimFromFoundItem  :: FoundItem
    ,   matchedId           :: MatchedItemId
    ,   claimedDate         :: UTCTime
    ,   claimant            :: Person

    , chalengeAnswers :: [ChallengeAnser]
    } deriving (Eq, Ord, Show)   

-- Revoir 
data Category = Category {
        categoryId          :: CategoryId
    ,   categoryType        :: CategoryType
    ,   parentalStatus      :: ParentalStatus
    ,   categoryDesc        :: LongDescription
    ,   subCategories :: Set Category   
    } deriving (Eq, Ord, Show)


data ParentalStatus =
      Parent
    | Sub 
    deriving (Eq, Ord, Show)


data CategoryType = 
      Humans
    | Documents
    | Electronics 
    | PersonalItems
    deriving (Eq, Ord, Show)

-- Etudier l'éventualité d'un module support independant pour les données de localisations
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
    = Adamaoua
    | Centre
    | Est
    | FarNorth
    | Littoral
    | North
    | NorthWest
    | South
    | SouthWest
    | West
    deriving (Eq, Ord, Show)


data Division
--    Adamaoua 
    = Djerem 
    | FaroEtDeo 
    | MayoBanyo 
    | Mbere 
    | Vina 
--    Centre
    | HauteSanaga 
    | Lekie 
    | MbamEtInoubou 
    | MbamEtKim 
    | MefouEtAfamba 
    | MefouEtAkono 
    | Mfoundi 
    | NyongEtKelle 
    | NyongEtMfoumou 
    | NyongEtSoo 
--    Est
    | BoumbaEtNgoko 
    | HautNyong 
    | Kadey 
    | LomEtDjerem 
--    Far North
    | Diamare 
    | LogoneEtChari 
    | MayoDanay 
    | MayoKani 
    | MayoSava 
    | MayoTsanaga 
--    Littoral
    | Moungo 
    | Nkam 
    | SanagaMaritime 
    | Wouri 
--    North

    | Benoue 
    | Faro 
    | MayoLouti 
    | MayoRey 
--    NorthWest
    | Boyo 
    | Bui 
    | DongaMantung 
    | Menchum 
    | Mezam 
    | Momo 
    | Ngoketunjia 
--    South
    | DjaEtLobo 
    | Mvila 
    | Ocean 
    | ValleeDuNtem 
--    SouthWest
    | Fako 
    | KoupeManengouba 
    | Lebialem 
    | Manyu 
    | Meme 
    | Ndian 
--    West
    | Bamboutos 
    | HautNkam 
    | HautsPlateaux 
    | KoungKhi 
    | Menoua 
    | Mifi 
    | Nde 
    | Noun 
    deriving (Eq, Ord, Show)

    
data SubDivision
-- ADAMAOUA --
--  Djerem
    = Gouandal 
    | Tibati 
--  Faro et Deo 
    | GalimTignere
    | MayoBaleo 
    | Tignere
--  Mayo Banyo
    | Bankim
    | Banyo
    | MayoDarl
-- Mbere
    | Dir
    | Djohong
    | Meiganga
    | Ngaoui
-- Vina 
    | Belel
    | Mbe
    | Nganha
    | NgaoundereIer
    | NgaoundereIIe
    | NgaoundereIIIe
    | Nyambaka
    | Martap
-- CENTRE --
-- Haute Sanaga
    | Bibey
    | LembeYezoum
    | Mbandjock
    | Minta
-- Lekie
    | Batchenga
    | Ebebda
    | EligMfomo
    | Evodoula
    | Lobo
    | Monatélé
    | Obala
    | Okola
    | Saa
-- Mbam Et Inoubou
    | Bafia
    | Bokito
    | Deuk
    | Kiiki
    | KonYambetta
    | Makenene
    | Ndikiniméki
    | Nitoukou
    | Ombessa
-- Mbam et Kim
    | Mbangassina
    | NgambeTikar
    | Ngoro
    | Ntui
    | Yoko
-- MefouEtAfamba 
    | Afanloum
    | AssambaOrOlanguina
    | Awae
    | Edzendouan
    | Esse
    | Mfou
    | Nkolafamba
    | Soa
-- MefouEtAkono 
    | Akono
    | Bikok
    | Mbankomo
    | Ngoumou
-- Mfoundi 
    | YaoundeIOrNlongkakOrEtoudi
    | YaoundeIIOrTsinga
    | YaoundeIIIOrEfoulan
    | YaoundeOrIVKondengui
    | YaoundeVOrEssos
    | YaoundeVIOrBiyemAssi
    | YaoundeVIIOrNkolbisson
-- NyongEtKelle 
    | Biyouha
    | Bondjock
    | BotMakak
    | Dibang
    | Eseka
    | Makak
    | Matomb
    | Messondo
    | NgogMapubi
    | Nguibassal
-- NyongEtMfoumou 

    | Akonolinga
    | Ayos
    | Endom
    | Mengang
    | NyakokomboOrKobdombo
-- NyongEtSoo 

    | Akoeman
    | Dzeng
    | Mbalmayo
    | Mengueme
    | Ngomedzap
    | Nkolmetet
-- EST --
-- BoumbaEtNgoko 
    | GariGombo
    | Moloundou
    | Salapoumbe
    | Yokadouma
-- HautNyong
    | AbongMbang
    | BebendOrAtok
    | Dimako
    | Doumaintang
    | Doume
    | Lomie
    | MboanzOrAngossas
    | Mboma
    | Messamena
    | Messok
    | Mindourou
    | Ngoyla
    | Nguelemendouka
    | Somalomo
-- Kadey 
    | Batouri
    | Kentzou
    | Kette
    | Mbang
    | Ndelele
    | Nguelebok
    | Ouli
-- LomEtDjerem 
    | Belabo
    | BertouaIer
    | BertouaIIe
    | BétareOya
    | Diang
    | GarouaBoulaï
    | Mandjou
    | Ngoura
-- FAR NORTH --
-- Diamare 
    | Bogo
    | Dargala
    | Gazawa
    | MarouaIer
    | MarouaIIe
    | MarouaIIIe
    | Meri
    | Ndoukoula
    | Pette
-- LogoneEtChari 
    | Blangoua
    | Darak
    | Fotokol
    | Goulfey
    | HileAlifa
    | Kousseri
    | LogoneBirni
    | Makary
    | Waza
    | Zina
-- MayoDanay 
    | Datcheka
    | Gobo
    | Gueme
    | Guere
    | KaiKai
    | Kalfou
    | KarHay
    | Maga
    | TchatiBali
    | Wina
    | Yagoua
-- MayoKani 
    | Dziguilao -- ??? Commune ou arrondiseement ???
    | Taibong
    | Guidiguis
    | Kaele
    | Mindif
    | Moulvoudaye
    | Moutourwa
    | Touloum
-- MayoSava
    | Kolofata
    | Mora
    | Tokombere
-- MayoTsanaga 
    | Bourrha
    | Hina
    | Koza
    | Mogode
    | Mokolo
    | Mozogo
    | SouledeRoua
-- LITTORAL --
-- Moungo 
    | BareBakem
    | Bonalea
    | Dibombari
    | Loum
    | Manjo
    | Mbanga
    | Melong
    | Mombo
    | NjombePenjaOrPenja
    | NkongsambaIer
    | NkongsambaIIe
    | NkongsambaIIIe
    | NlonakoOrEbone
-- Nkam 
    | Nkondjock
    | NordMakombeOrNdobian
    | Yabassi
    | Yingui
-- SanagaMaritime 
    | Dibamba
    | Dizangue
    | EdeaIer
    | EdeaIIe
    | MassockSongloulou
    | Mouanko
    | Ndom
    | Ngambe
    | Ngwei
    | Nyanon
    | Pouma
-- Wouri 
    | DoualaIer
    | DoualaIIe
    | DoualaIIIe
    | DoualaIVe
    | DoualaVe
    | DoualaVIe
    | Manoka
-- NORTH --
-- Benoue 
    | Bibemi
    | Dembo
    | GarouaIer
    | GarouaIIe
    | GarouaIIIe
    | Lagdo
    | MayoHourna
    | Pitoa
    | Tcheboa
    | Demsa
    | Bascheo
-- Faro 
    | Beka
    | Poli
-- MayoLouti 
    | Figuil
    | Guider
    | MayoOulo
-- MayoRey 
    | Madingring
    | ReyBouba
    | Tchollire
    | Touboro
-- NORTH WEST --
-- Boyo
    | Belo
    | Fonfuka
    | Fundong
    | Njinikom
-- Bui 
    | Jakiri
    | Kumbo
    | MbvenOrMbiame
    | Nkum
    | NoniOrNkor
    | OkuOrElakOku
-- DongaMantung 
    | Ako
    | Misaje
    | Ndu
    | Nkambe
    | Nwa
-- Menchum 
    | Benakuma
    | FuruAwa
    | Wum
    | Zhoa
-- Mezam 
    | Bafut
    | Bali
    | BamendaIer
    | BamendaIIe
    | BamendaIIIe
    | Santa
    | Tubah
-- Momo 
    | Andek
    | Batibo
    | Mbengwi
    | Njikwa
    | WidikumBoffe
-- Ngoketunjia

    | Babessi
    | Balikumbat
    | Ndop
-- SOUTH 
-- DjaEtLobo 
    | Bengbis
    | Djoum
    | Meyomessala
    | Meyomessi
    | Mintom
    | Oveng
    | Sangmelima
    | Zoetele
-- Mvila
    | BiwongBane
    | BiwongBulu
    | EbolowaIer
    | EbolowaIIe
    | Efoulan
    | Mengong
    | Mvangan
    | Ngoulemakong
-- Ocean
    | AkomII
    | Bipindi
    | Campo
    | KribiIer
    | KribiIIe
    | Lokoundje
    | Lolodorf
    | Mvengue
    | Niete
-- ValleeDuNtem
    | Ambam
    | KyeOssi
    | Maan
    | Olamze
-- SOUTH WEST --
-- Fako
    | Buea
    | LimbeIer
    | LimbeIIe
    | LimbeIIIe
    | Muyuka
    | Tiko
    | WestCoast
-- KoupeManengouba
    | Bangem
    | Nguti
    | Tombel
-- Lebialem 
    | Alou
    | Fontem
    | Wabane
-- Manyu
    | Akwaya
    | Eyumodjock
    | Mamfe
    | TintoOrUpperBayang
-- Meme 
    | Konye
    | KumbaIer
    | KumbaIIe
    | KumbaIIIe
    | Mbonge
-- Ndian
    | Bamusso
    | EkondoTiti
    | Idabato
    | Isanguele
    | KomboAbedimo
    | KomboItindi
    | Mundemba
-- WEST -- 
-- Bamboutos
    | Babadjou
    | Batcham
    | Galim
    | Mbouda
-- Haut Nkam 
    | Bafang
    | Bakou
    | Bana
    | Bandja
    | Banka
    | Kekem
-- Hauts Plateaux
    | Baham
    | Bamendjou
    | Bangou
    | Batie
    | Bangam
    | Badenkop
-- KoungKhi
    | Bayangam
    | Demdeng
    | Poumougne
-- Menoua 
    | Dschang
    | Fokoue
    | FongoTongo
    | NkongZem
    | PenkaMichel
    | Santchou
-- Mifi
    | BafoussamIer
    | BafoussamIIe
    | BafoussamIIIe
-- Nde
    | Bangangte
    | Bassamba
    | Bazou
    | Tonga
-- Noun 
    | Bangourain
    | Foumban
    | Foumbot
    | Kouoptamo
    | Koutaba
    | Magba
    | Malentouen
    | Massangam
    | Njimom
    deriving (Eq, Ord, Show)

-- Definir les attributs relativement aux Categories "Didier"
data Attribute = Attribute {
      attrCode             :: AttributeCode
    , attrName             :: AttributeName
    , attrDescription      :: ShortDescription
    , attrValue            :: Maybe AttributeValue
    , attrUnit             ::   Maybe AttributeUnit
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


data Challenge = Question deriving (Eq, Show, Ord) 
data ChallengeAnser = 
    ChallengeAnser (Question, Answer) deriving (Eq, Show, Ord)






--- Helper functions
---
---


--- Location helper
toRegion :: String -> Either ErrorMessage Region
toRegion str  
    | "adamaoua" == lowerStr = Right Adamaoua
    | "centre" == lowerStr = Right Centre
    | "est" == lowerStr = Right Est
    | "farnorth" == lowerStr = Right FarNorth
    | "littoral" == lowerStr = Right Littoral
    | "north" == lowerStr = Right North
    | "northwest" == lowerStr = Right NorthWest
    | "south" == lowerStr = Right South
    | "southwest" == lowerStr = Right SouthWest
    | "west" == lowerStr = Right West
    | otherwise  = Left $ str <> ": is an invalid region code"
    where lowerStr = fmap toLower str

fromRegion :: Region -> String
fromRegion region = 
    case region of
        Adamaoua -> "Adamaoua"
        Centre -> "Centre"
        Est -> "Est"
        FarNorth -> "FarNorth"
        Littoral -> "Littoral"
        North -> "North"
        NorthWest -> "NorthWest"
        South -> "South"
        SouthWest -> "SouthWest"
        West -> "West"
   


toDivision :: String -> Either ErrorMessage Division
toDivision str
    | "faroetdeo" == lowerStr = Right FaroEtDeo 
    | "mayobanyo" == lowerStr = Right MayoBanyo 
    | "mbere" == lowerStr = Right Mbere 
    | "vina" == lowerStr = Right Vina 
    | otherwise = Left $ str <> ": is an invalid division code"
    -- TODO FINISH  ALL CASES
    where lowerStr = fmap toLower str


fromDivision :: Division -> String
fromDivision division = 
    case division of
        FaroEtDeo -> "FaroEtDeo"
        MayoBanyo -> "MayoBanyo"
        Mbere -> "Mbere"
        Vina -> "Vina"
        _ -> error "NOT IMPLEMENTED YET"


toSubDivision :: String -> Either ErrorMessage SubDivision
toSubDivision str  
    | "gouandal" == lowerStr = Right Gouandal
    | "tibati" == lowerStr = Right Tibati
    | otherwise  = Left $ str <> ": is an invalid sub division code"
    -- TODO FINISH  ALL CASES
    where lowerStr = fmap toLower str
   
--- Attribute data ???? should it be here???

fromSubDivision :: SubDivision -> String
fromSubDivision subdivision = 
    case subdivision of
        Gouandal -> "Gouandal"
        Tibati -> "Tibati"
        _ -> error "NOT IMPLEMENTED YET"

--- Category helper

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

--- Item helpers

itemTypeToString :: Item -> String
itemTypeToString item = 
    case item of
        Lost li -> "Lost"
        Found fi -> "Found"
        Matched mi -> "Matched"
        Claimed ci -> "Claimed"
        










--- Helpers 




 