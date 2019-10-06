module DomainTypes where



import DomainCommonTypes

import qualified Data.Map as M
import Data.Set
import Data.Time



data Item = 
      Lost RegisteredLostItem
    | Found DeclaredFoundItem
    | Matched LostAndFoundItem
    | Claimed ClaimedFoundItem


data RegisteredLostItem = RegisteredLostItem {
        liIdentifier :: Identifier
    ,   liName :: Name
    ,   liCategoryId :: Identifier
    ,   lostLocation :: Location
    ,   liDescription :: Description
    ,   liLostDate :: UTCTime
    ,   liAttributes :: [Attribute]
    ,   liOwner :: Person
    } deriving (Eq, Ord, Show)


data DeclaredFoundItem = DeclaredFoundItem {
        fiIdentifier :: Identifier
    ,   fiName :: Name
    ,   fiCategoryId :: CategoryIdentifier
    ,   foundLocation :: Location
    ,   fiDescription :: Description
    ,   fiFoundDate :: UTCTime
    ,   fiAttributes :: [Attribute]
    ,   fiDeclarant :: Person
    ,   fiStoker :: Tenant

    ,   chalenges :: [Challenge]
    } deriving (Eq, Ord, Show)


data LostAndFoundItem = LostAndFoundItem {
        lfiIdentifier :: Identifier
    ,   registeredLostItem :: RegisteredLostItem
    ,   declaredFoundItems :: [DeclaredFoundItem]
    } deriving (Eq, Ord, Show)  


data ClaimedFoundItem = ClaimedFoundItem {
        clIdentifier :: Identifier
    ,   clRegisteredLostItem :: RegisteredLostItem
    ,   clDeclaredFoundItem :: DeclaredFoundItem
    ,   matchedId :: Identifier
    ,   claimedDate :: UTCTime
    ,   clClaimer :: Person

    , chalengeAnswers :: [ChallengeAnser]
    } deriving (Eq, Ord, Show)   

-- Revoir 
data Category = Category {
        categoryIdentifier :: CategoryIdentifier
    ,   categoryType :: CategoryType
    ,   parentalStatus :: ParentalStatus
    ,   categoryDescription :: CategoryDescription
    ,   subCategories :: Set Category   
    } deriving (Eq, Ord, Show)


data ParentalStatus =
      Parent
    | Sub 
    deriving (Eq, Ord, Show)

-- Revoir
data CategoryType = 
      Humans
    | Documents
    | Electronics 
    | PersonalItems
    deriving (Eq, Ord, Show)


-- Revoir les disctricts
-- Etudier l'éventualité d'un module support independant pour les donnees de localisations
data Location = Location {
        region :: Region
    ,   division :: Division
    ,   subdivision :: SubDivision
    ,   city :: City
    ,   village :: Village
    ,   neighborhood :: Neighborhood
    ,   loAddress :: Address
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
    = Djerem String
    | FaroEtDeo String
    | MayoBanyo String
    | Mbere String
    | Vina String

--    Centre
    | HauteSanaga String
    | Lekie String
    | MbamEtInoubou String
    | MbamEtKim String
    | MefouEtAfamba String
    | MefouEtAkono String
    | Mfoundi String
    | NyongEtKelle String
    | NyongEtMfoumou String
    | NyongEtSoo String

--    Est
    | BoumbaEtNgoko String
    | HautNyong String
    | Kadey String
    | LomEtDjerem String

--    Far North
    | Diamare String
    | LogoneEtChari String
    | MayoDanay String
    | MayoKani String
    | MayoSava String
    | MayoTsanaga String

--    Littoral
    | Moungo String
    | Nkam String
    | SanagaMaritime String
    | Wouri String

--    North

    | Benoue String
    | Faro String
    | MayoLouti String
    | MayoRey String

--    NorthWest
    | Boyo String
    | Bui String
    | DongaMantung String
    | Menchum String
    | Mezam String
    | Momo String
    | Ngoketunjia String

--    South
    | DjaEtLobo String
    | Mvila String
    | Ocean String
    | ValleeDuNtem String

--    SouthWest
    | Fako String
    | KoupeManengouba String
    | Lebialem String
    | Manyu String
    | Meme String
    | Ndian String

--    West
    | Bamboutos String
    | HautNkam String
    | HautsPlateaux String
    | KoungKhi String
    | Menoua String
    | Mifi String
    | Nde String
    | Noun String
    deriving (Eq, Ord, Show)

    


data SubDivision
-- ADAMAOUA

--  Djerem
    = Gouandal String
    | Tibati String
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


-- CENTRE 


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

-- EST

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

-- FAR NORTH

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

-- LITTORAL

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


-- NORTH


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
 

-- NORTH WEST


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



-- SOUTH WEST


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




-- WEST



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






-- Definir les attributs relativement aux Categories 
data Attribute = Attribute {
        attrCode :: AttributeCode
      , attrName :: AttributeName
      , attrDescription :: Description
      , attrValue :: Maybe AttributeValue
      , attrUnit ::   Maybe AttributeUnit
      , relatedCategory :: CategoryIdentifier
      , relatedCategoryType :: CategoryType
    } deriving (Eq, Ord, Show)


data Person = Person {
    -- Revoir si user est optionelle
      user :: Identifier
    , contact :: ContactInformation
    , name :: FullName
    } deriving (Eq, Ord, Show)


data ContactInformation = ContactInformation {
      -- Tel required, email optional
      email :: EmailAddress
    , address :: PostalAddress
    , primaryTel :: Telephone
    , secondaryTel :: Telephone
    } deriving (Eq, Ord, Show)


data FullName = FullName {
      first :: FirstName
    , middle :: Maybe Middle
    , last :: LastName
    } deriving (Eq, Ord, Show)





data Tenant = Tenant {
      tenantId :: Identifier 
    , tenantName :: Name
    , tenantDescription :: Description
    , tenantContactAddress :: ContactInformation
    } deriving (Eq, Show, Ord)





data Challenge = Question deriving (Eq, Show, Ord) 
data ChallengeAnser = 
    ChallengeAnser (Question, Answer) deriving (Eq, Show, Ord)









---------- TESTING ---------
{-- 
regLostItem1 :: Item 
regLostItem1 = Lost RegisteredLostItem {
      liIdentifier = Identifier "regLostItem1"
    , liName = Name "Car keys"
    , liCategoryId = CategoryIdentifier (Identifier "category1")  PersonalItems
    , lostLocation = Location { 
           region = Adamaoua
        ,  division = Djerem "Djerem"
        ,  subdivision = Gouandal "Gouandal" 
        ,  city = City "Ngaoundere"
        ,  village = Village "Ngaoundere"
        ,  neighborhood = Neighborhood "Chefferie"
        ,  loAddress = Address "Rond point 3 morts"
        }
    , liDescription = 
        Description "Mes cles sont attachees a un ruban rouge et la marque de ma voiture c'est toyota"
    , liLostDate = "21/01/1985"
    , liAttributes = [Color "red", Brand "Toyota", Size  "smal"] 
}
regLostItem2 :: Item
regLostItem2 = Lost RegisteredLostItem {
      liIdentifier = Identifier "regLostItem2"
    , liName = Name "National Identity Card"
    , liCategoryId = CategoryIdentifier (Identifier "category2") PersonalItems
    , lostLocation = Location { 
           region = Adamaoua
        ,  division = Djerem "Djerem" 
        ,  subdivision = Gouandal "Gouandal"
        ,  city = City "Ngaoundere"
        ,  village = Village "Ngaoundere"
        ,  neighborhood = Neighborhood "Chefferie"
        ,  loAddress = Address "Carrefour 3 bléssés"
        }
    , liDescription = 
        Description "Ma carte nationale d'identité est encore l'acien format"
    , liLostDate = "22/01/1985"
    , liAttributes = [] 
}

getItemIdentifier :: Item -> Identifier
getItemIdentifier elem =
    case elem of
        Lost item -> liIdentifier item
        Found item -> fiIdentifier item
        Matched item -> lfiIdentifier item
        Claimed item -> clIdentifier item


regLostItems :: M.Map Identifier Item
regLostItems = M.fromList [
    (getItemIdentifier regLostItem1, regLostItem1), 
    (getItemIdentifier regLostItem2, regLostItem2)
    ]
--}







