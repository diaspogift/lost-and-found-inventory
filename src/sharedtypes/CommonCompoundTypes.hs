module CommonCompoundTypes where

import CommonSimpleTypes 
import Data.Char (
    toLower
    )
import Data.Either (
    rights
    )
import Data.Set (
    Set, 
    fromList, 
    toList)


-- ==============================================================================================
-- Common compound types used throughout the Inventory Management domain
--
-- Includes: Categories, Locations, Regions, Divisons, SubDivisions etc.
--
-- ==============================================================================================


-- ===============================================================
-- Category-related types
-- ===============================================================


data Category
  = RootCategory CategoryInfo
  | SubCategory CategoryInfo (Maybe ParentInfo)
  deriving (Eq, Ord, Show)

data CategoryInfo
  = CategoryInfo
      { categoryId :: CategoryId,
        categoryCode :: CategoryCode,
        categoryEnablementStatus :: EnablementStatus,
        categoryDescription :: LongDescription,
        categoryRelatedSubCategories :: Set CategoryId
      }
  deriving (Eq, Ord, Show)

data AddedSubCategory
  = AddedSubCategory
      { addedSubCategoryParent :: ParentCategoryId,
        addSubCategoryId :: SubCategoryId
      }
  deriving (Eq, Ord, Show)

data ParentInfo
  = ParentInfo
      { parentInfoId :: ParentCategoryId,
        parentInfoCode :: CategoryCode
      }
  deriving (Eq, Ord, Show)

data EnablementStatus
  = Enabled Reason
  | Disabled Reason
  deriving (Eq, Ord, Show)


toEnablementStatus :: (String, String) -> Either ErrorMessage EnablementStatus
toEnablementStatus (enblmntType, reason)
  | enblmntType == "Enabled" && notNull reason =
    return . Enabled $ reason
  | enblmntType == "Disabled" && notNull reason =
    return . Disabled $ reason
  | otherwise =
    Left $
      "inconsistent data format of "
        <> "("
        <> enblmntType
        <> ","
        <> reason
        <> ")"

fromEnblmntStatus :: EnablementStatus -> (String, String)
fromEnblmntStatus (Enabled reason) = ("Enabled", reason)
fromEnblmntStatus (Disabled reason) = ("Disabled", reason)

-- ===============================================================
-- Location-related types
-- ===============================================================

data Location
  = Location
      { locationAdminArea :: Maybe AdministrativeAreaInfo,
        locationCityOrVillage :: Maybe CityOrVillage,
        locationNeighborhood :: Maybe Neighborhood,
        locationAddresses :: [Address]
      }
  deriving (Eq, Ord, Show)

type AdministrativeAreaInfo = (Region, Division, SubDivision)

-- ===============================================================
-- Region-related types
-- ===============================================================

data Region
  = Adamaoua
  | Centre
  | East
  | FarNorth
  | Littoral
  | North
  | NorthWest
  | South
  | SouthWest
  | West
  deriving (Eq, Ord)

instance Show Region where
  show reg =
    case reg of
      Adamaoua -> "Adamaoua"
      Centre -> "Centre"
      East -> "East"
      FarNorth -> "Far North"
      Littoral -> "Littoral"
      North -> "North"
      NorthWest -> "North West"
      South -> "South"
      SouthWest -> "South West"
      West -> "West"



-- ===============================================================
-- Division-related types
-- ===============================================================



data Division
  = --    Adamaoua
    -- Consider a show instance
    Djerem
  | FaroEtDeo
  | MayoBanyo
  | Mbere
  | Vina
  | --    Centre
    HauteSanaga
  | Lekie
  | MbamEtInoubou
  | MbamEtKim
  | MefouEtAfamba
  | MefouEtAkono
  | Mfoundi
  | NyongEtKelle
  | NyongEtMfoumou
  | NyongEtSoo
  | --    Est
    BoumbaEtNgoko
  | HautNyong
  | Kadey
  | LomEtDjerem
  | --    Far North
    Diamare
  | LogoneEtChari
  | MayoDanay
  | MayoKani
  | MayoSava
  | MayoTsanaga
  | --    Littoral
    Moungo
  | Nkam
  | SanagaMaritime
  | Wouri
  | --    North

    Benoue
  | Faro
  | MayoLouti
  | MayoRey
  | --    NorthWest
    Boyo
  | Bui
  | DongaMantung
  | Menchum
  | Mezam
  | Momo
  | Ngoketunjia
  | --    South
    DjaEtLobo
  | Mvila
  | Ocean
  | ValleeDuNtem
  | --    SouthWest
    Fako
  | KoupeManengouba
  | Lebialem
  | Manyu
  | Meme
  | Ndian
  | --    West
    Bamboutos
  | HautNkam
  | HautsPlateaux
  | KoungKhi
  | Menoua
  | Mifi
  | Nde
  | Noun
  deriving (Eq, Ord)

instance Show Division where
  show div =
    case div of
      -- Adamaoua
      Djerem -> show "Djerem"
      FaroEtDeo -> show "Faro et Déo"
      MayoBanyo -> show "Mayo Banyo"
      Mbere -> show "Mbéré"
      Vina -> show "Vina"
      HauteSanaga -> show "Haute Sanaga"
      Lekie -> show "Lekié"
      MbamEtInoubou -> show "Mbam et Inoubou"
      MbamEtKim -> show "Mbam et Kim"
      MefouEtAfamba -> show "Mefou et Afamba"
      MefouEtAkono -> show "Mefou et Akono"
      Mfoundi -> show "Mfoundi"
      NyongEtKelle -> show "Nyong et Kelle"
      NyongEtMfoumou -> show "Nyong et Mfoumou"
      NyongEtSoo -> show "Nyong et Soo"
      BoumbaEtNgoko -> show "Boumba et Ngoko"
      HautNyong -> show "Haut Nyong"
      Kadey -> show "Kadey"
      LomEtDjerem -> show "Lom et Djerem"
      Diamare -> show "Diamare"
      LogoneEtChari -> show "Logone et Chari"
      MayoDanay -> show "Mayo Danay"
      MayoKani -> show "Mayo Kani"
      MayoSava -> show "Mayo Sava"
      MayoTsanaga -> show "Mayo Tsanaga"
      Moungo -> show "Moungo"
      Nkam -> show "Nkam"
      SanagaMaritime -> show "Sanaga Maritime"
      Wouri -> show "Wouri"
      Benoue -> show "Benoue"
      Faro -> show "Faro"
      MayoRey -> show "Mayo Rey"
      MayoLouti -> show "Mayo Louti"
      Boyo -> show "Boyo"
      Bui -> show "Bui"
      Menchum -> show "Menchum"
      DongaMantung -> show "Donga Mantung"
      Mezam -> show "Mezam"
      Momo -> show "Momo"
      Ngoketunjia -> show "Ngoketunjia"
      DjaEtLobo -> show "Dja et Lobo"
      Mvila -> show "Mvila"
      ValleeDuNtem -> show "Vallée du Ntem"
      Ocean -> show "Ocean"
      KoupeManengouba -> show "Koupé Manengouba"
      Fako -> show "Fako"
      Lebialem -> show "Lebialem"
      Manyu -> show "Manyu"
      Meme -> show "Meme"
      Ndian -> show "Ndian"
      Bamboutos -> show "Bamboutos"
      HautNkam -> show "Haut Nkam"
      HautsPlateaux -> show "Hauts Plateaux"
      KoungKhi -> show "KoungKhi"
      Menoua -> show "Menoua"
      Mifi -> show "Mifi"
      Nde -> show "Nde"
      Noun -> show "Noun"



-- ===============================================================
-- SubDivision-related types
-- ===============================================================



data SubDivision
  = -- ADAMAOUA --
    --  Djerem
    Gouandal
  | Tibati
  | --  Faro et Deo
    GalimTignere
  | MayoBaleo
  | Tignere
  | --  Mayo Banyo
    Bankim
  | Banyo
  | MayoDarl
  | -- Mbere
    Dir
  | Djohong
  | Meiganga
  | Ngaoui
  | -- Vina
    Belel
  | Mbe
  | Nganha
  | NgaoundereIer
  | NgaoundereIIe
  | NgaoundereIIIe
  | Nyambaka
  | Martap
  | -- CENTRE --
    -- Haute Sanaga
    Bibey
  | LembeYezoum
  | Mbandjock
  | Minta
  | -- Lekie
    Batchenga
  | Ebebda
  | EligMfomo
  | Evodoula
  | Lobo
  | Monatele
  | Obala
  | Okola
  | Saa
  | -- Mbam Et Inoubou
    Bafia
  | Bokito
  | Deuk
  | Kiiki
  | KonYambetta
  | Makenene
  | Ndikiniméki
  | Nitoukou
  | Ombessa
  | -- Mbam et Kim

    Mbangassina
  | NgambeTikar
  | Ngoro
  | Ntui
  | Yoko
  | -- MefouEtAfamba

    Afanloum
  | AssambaOrOlanguina
  | Awae
  | Edzendouan
  | Esse
  | Mfou
  | Nkolafamba
  | Soa
  | -- MefouEtAkono
    Akono
  | Bikok
  | Mbankomo
  | Ngoumou
  | -- Mfoundi
    YaoundeIOrNlongkakOrEtoudi
  | YaoundeIIOrTsinga
  | YaoundeIIIOrEfoulan
  | YaoundeOrIVKondengui
  | YaoundeVOrEssos
  | YaoundeVIOrBiyemassi
  | YaoundeVIIOrNkolbisson
  | -- NyongEtKelle
    Biyouha
  | Bondjock
  | BotMakak
  | Dibang
  | Eseka
  | Makak
  | Matomb
  | Messondo
  | NgogMapubi
  | Nguibassal
  | -- NyongEtMfoumou

    Akonolinga
  | Ayos
  | Endom
  | Mengang
  | NyakokomboOrKobdombo
  | -- NyongEtSoo

    Akoeman
  | Dzeng
  | Mbalmayo
  | Mengueme
  | Ngomedzap
  | Nkolmetet
  | -- EST --
    -- BoumbaEtNgoko
    GariGombo
  | Moloundou
  | Salapoumbe
  | Yokadouma
  | -- HautNyong
    AbongMbang
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
  | -- Kadey

    Batouri
  | Kentzou
  | Kette
  | Mbang
  | Ndelele
  | Nguelebok
  | Ouli
  | -- LomEtDjerem

    Belabo
  | BertouaIer
  | BertouaIIe
  | BetareOya
  | Diang
  | GarouaBoulai
  | Mandjou
  | Ngoura
  | -- FAR NORTH --

    -- Diamare
    Bogo
  | Dargala
  | Gazawa
  | MarouaIer
  | MarouaIIe
  | MarouaIIIe
  | Meri
  | Ndoukoula
  | Pette
  | -- LogoneEtChari

    Blangoua
  | Darak
  | Fotokol
  | Goulfey
  | HileAlifa
  | Kousseri
  | LogoneBirni
  | Makary
  | Waza
  | Zina
  | -- MayoDanay
    Datcheka
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
  | -- MayoKani
    Dziguilao -- ??? Commune ou arrondiseement ???
  | Taibong
  | Guidiguis
  | Kaele
  | Mindif
  | Moulvoudaye
  | Moutourwa
  | Touloum
  | -- MayoSava
    Kolofata
  | Mora
  | Tokombere
  | -- MayoTsanaga
    Bourrha
  | Hina
  | Koza
  | Mogode
  | Mokolo
  | Mozogo
  | SouledeRoua
  | -- LITTORAL --
    -- Moungo
    BareBakem
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
  | -- Nkam
    Nkondjock
  | NordMakombeOrNdobian
  | Yabassi
  | Yingui
  | -- SanagaMaritime
    Dibamba
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
  | -- Wouri

    DoualaIer
  | DoualaIIe
  | DoualaIIIe
  | DoualaIVe
  | DoualaVe
  | DoualaVIe
  | Manoka
  | -- NORTH --
    -- Benoue
    Bibemi
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
  | -- Faro
    Beka
  | Poli
  | -- MayoLouti
    Figuil
  | Guider
  | MayoOulo
  | -- MayoRey
    Madingring
  | ReyBouba
  | Tchollire
  | Touboro
  | -- NORTH WEST --
    -- Boyo

    Belo
  | Fonfuka
  | Fundong
  | Njinikom
  | -- Bui
    Jakiri
  | Kumbo
  | MbvenOrMbiame
  | Nkum
  | NoniOrNkor
  | OkuOrElakOku
  | -- DongaMantung
    Ako
  | Misaje
  | Ndu
  | Nkambe
  | Nwa
  | -- Menchum
    Benakuma
  | FuruAwa
  | Wum
  | Zhoa
  | -- Mezam
    Bafut
  | Bali
  | BamendaIer
  | BamendaIIe
  | BamendaIIIe
  | Santa
  | Tubah
  | -- Momo
    Andek
  | Batibo
  | Mbengwi
  | Njikwa
  | WidikumBoffe
  | -- Ngoketunjia

    Babessi
  | Balikumbat
  | Ndop
  | -- SOUTH
    -- DjaEtLobo
    Bengbis
  | Djoum
  | Meyomessala
  | Meyomessi
  | Mintom
  | Oveng
  | Sangmelima
  | Zoetele
  | -- Mvila
    BiwongBane
  | BiwongBulu
  | EbolowaIer
  | EbolowaIIe
  | Efoulan
  | Mengong
  | Mvangan
  | Ngoulemakong
  | -- Ocean

    AkomII
  | Bipindi
  | Campo
  | KribiIer
  | KribiIIe
  | Lokoundje
  | Lolodorf
  | Mvengue
  | Niete
  | -- ValleeDuNtem
    Ambam
  | KyeOssi
  | Maan
  | Olamze
  | -- SOUTH WEST --
    -- Fako
    Buea
  | LimbeIer
  | LimbeIIe
  | LimbeIIIe
  | Muyuka
  | Tiko
  | WestCoast
  | -- KoupeManengouba
    Bangem
  | Nguti
  | Tombel
  | -- Lebialem
    Alou
  | Fontem
  | Wabane
  | -- Manyu
    Akwaya
  | Eyumodjock
  | Mamfe
  | TintoOrUpperBayang
  | -- Meme
    Konye
  | KumbaIer
  | KumbaIIe
  | KumbaIIIe
  | Mbonge
  | -- Ndian
    Bamusso
  | EkondoTiti
  | Idabato
  | Isanguele
  | KomboAbedimo
  | KomboItindi
  | Mundemba
  | -- WEST --
    -- Bamboutos
    Babadjou
  | Batcham
  | Galim
  | Mbouda
  | -- Haut Nkam
    Bafang
  | Bakou
  | Bana
  | Bandja
  | Banka
  | Kekem
  | -- Hauts Plateaux
    Baham
  | Bamendjou
  | Bangou
  | Batie
  | Bangam
  | Badenkop
  | -- KoungKhi
    Bayangam
  | Demdeng
  | Poumougne
  | -- Menoua
    Dschang
  | Fokoue
  | FongoTongo
  | NkongZem
  | PenkaMichel
  | Santchou
  | -- Mifi

    BafoussamIer
  | BafoussamIIe
  | BafoussamIIIe
  | -- Nde
    Bangangte
  | Bassamba
  | Bazou
  | Tonga
  | -- Noun
    Bangourain
  | Foumban
  | Foumbot
  | Kouoptamo
  | Koutaba
  | Magba
  | Malentouen
  | Massangam
  | Njimom
  deriving (Eq, Ord)

instance Show SubDivision where
  show sub =
    case sub of
      Gouandal -> "Gouandal"
      Tibati -> "Tibati"
      --  Faro et Deo
      GalimTignere -> "GalimTignere"
      MayoBaleo -> "Mayo Baléo"
      Tignere -> "Tignere"
      --  Mayo Banyo
      Bankim -> "Bankim"
      Banyo -> "Banyo"
      MayoDarl -> "MayoDarl"
      -- Mbere
      Dir -> "Dir"
      Djohong -> "Djohong"
      Meiganga -> "Meiganga"
      Ngaoui -> "Ngaoui"
      -- Vina
      Belel -> "Belel"
      Mbe -> "Mbe"
      Nganha -> "Nganha"
      NgaoundereIer -> "Ngaoundere Ier"
      NgaoundereIIe -> "Ngaoundere IIe"
      NgaoundereIIIe -> "Ngaoundere IIIe"
      Nyambaka -> "Nyambaka"
      Martap -> "Martap"
      -- Centre
      -- Haute Sanaga
      Bibey -> "Bibey"
      LembeYezoum -> "Lembe Yezoum"
      Mbandjock -> "Mbandjock"
      Minta -> "Minta"
      -- Lekie
      Batchenga -> "Batchenga"
      Ebebda -> "Ebebda"
      EligMfomo -> "Elig Mfomo"
      Evodoula -> "Evodoula"
      Lobo -> "Lobo"
      Monatele -> "Monatélé"
      Obala -> "Obala"
      Okola -> "Okola"
      Saa -> "Saa"
      -- Mbam Et Inoubou
      Bafia -> "Bafia"
      Bokito -> "Bokito"
      Deuk -> "Deuk"
      Kiiki -> "Kiiki"
      KonYambetta -> "Kon Yambetta"
      Makenene -> "Makenene"
      Ndikiniméki -> "Ndikiniméki"
      Nitoukou -> "Nitoukou"
      Ombessa -> "Ombessa"
      -- Mbam et Kim
      Mbangassina -> "Mbangassina"
      NgambeTikar -> "Ngambe Tikar"
      Ngoro -> "Ngoro"
      Ntui -> "Ntui"
      Yoko -> "Yoko"
      -- MefouEtAfamba
      Afanloum -> "Afanloum"
      AssambaOrOlanguina -> "Assamba / Olanguina"
      Awae -> "Awae"
      Edzendouan -> "Edzendouan"
      Esse -> "Esse"
      Mfou -> "Mfou"
      Nkolafamba -> "Nkolafamba"
      Soa -> "Soa"
      -- MefouEtAkono
      Akono -> "Akono"
      Bikok -> "Bikok"
      Mbankomo -> "Mbankomo"
      Ngoumou -> "Ngoumou"
      -- Mfoundi
      YaoundeIOrNlongkakOrEtoudi -> "Yaounde I / Nlongkak / Etoudi"
      YaoundeIIOrTsinga -> "Yaounde II / Tsinga"
      YaoundeIIIOrEfoulan -> "Yaounde III / Efoulan"
      YaoundeOrIVKondengui -> "Yaounde IV / Kondengui"
      YaoundeVOrEssos -> "Yaounde V / Essos"
      YaoundeVIOrBiyemassi -> "Yaounde VI / BiyemAssi"
      YaoundeVIIOrNkolbisson -> "Yaounde VII / Nkolbisson"
      -- NyongEtKelle
      Biyouha -> "Biyouha"
      Bondjock -> "Bondjock"
      BotMakak -> "BotMakak"
      Dibang -> "Dibang"
      Eseka -> "Eseka"
      Makak -> "Makak"
      Matomb -> "Matomb"
      Messondo -> "Messondo"
      NgogMapubi -> "Ngog Mapubi"
      Nguibassal -> "Nguibassal"
      -- NyongEtMfoumou
      Akonolinga -> "Akonolinga"
      Ayos -> "Ayos"
      Endom -> "Endom"
      Mengang -> "Mengang"
      NyakokomboOrKobdombo -> "Nyakokombo / Kobdombo"
      -- NyongEtSoo
      Akoeman -> "Akoeman"
      Dzeng -> "Dzeng"
      Mbalmayo -> "Mbalmayo"
      Mengueme -> "Mengueme"
      Ngomedzap -> "Ngomedzap"
      Nkolmetet -> "Nkolmetet"
      -- East
      -- BoumbaEtNgoko
      GariGombo -> "Gari Gombo"
      Moloundou -> "Moloundou"
      Salapoumbe -> "Salapoumbe"
      Yokadouma -> "Yokadouma"
      -- HautNyong
      AbongMbang -> "Abong Mbang"
      BebendOrAtok -> "Bebend / Atok"
      Dimako -> "Dimako"
      Doumaintang -> "Doumaintang"
      Doume -> "Doume"
      Lomie -> "Lomie"
      MboanzOrAngossas -> "Mboanz / Angossas"
      Mboma -> "Mboma"
      Messamena -> "Messamena"
      Messok -> "Messok"
      Mindourou -> "Mindourou"
      Ngoyla -> "Ngoyla"
      Nguelemendouka -> "Nguelemendouka"
      Somalomo -> "Somalomo"
      -- Kadey
      Batouri -> "Batouri"
      Kentzou -> "Kentzou"
      Kette -> "Kette"
      Mbang -> "Mbang"
      Ndelele -> "Ndelele"
      Nguelebok -> "Nguelebok"
      Ouli -> "Ouli"
      -- LomEtDjerem
      Belabo -> "Belabo"
      BertouaIer -> "Bertoua Ier"
      BertouaIIe -> "Bertoua IIe"
      BetareOya -> "Bétare Oya"
      Diang -> "Diang"
      GarouaBoulai -> "Garoua Boulai"
      Ngoura -> "Ngoura"
      -- Far North
      -- Diamare
      Bogo -> "Bogo"
      Dargala -> "Dargala"
      Gazawa -> "Gazawa"
      MarouaIer -> "Maroua Ier"
      MarouaIIe -> "Maroua IIe"
      MarouaIIIe -> "Maroua IIIe"
      Meri -> "Meri"
      Ndoukoula -> "Ndoukoula"
      Pette -> "Pette"
      -- LogoneEtChari
      Blangoua -> "Blangoua"
      Darak -> "Darak"
      Fotokol -> "Fotokol"
      Goulfey -> "Goulfey"
      HileAlifa -> "Hile Alifa"
      Kousseri -> "Kousseri"
      LogoneBirni -> "Logone Birni"
      Makary -> "Makary"
      Waza -> "Waza"
      Zina -> "Zina"
      -- MayoDanay
      Datcheka -> "Datcheka"
      Gobo -> "Gobo"
      Gueme -> "Gueme"
      Guere -> "Guere"
      KaiKai -> "KaiKai"
      Kalfou -> "Kalfou"
      KarHay -> "KarHay"
      Maga -> "Maga"
      TchatiBali -> "Tchati Bali"
      Wina -> "Wina"
      Yagoua -> "Yagoua"
      -- MayoKani
      Dziguilao -> "Dziguilao" -- ??? Commune ou arrondiseement ???
      Taibong -> "Taibong"
      Guidiguis -> "Guidiguis"
      Kaele -> "Kaele"
      Mindif -> "Mindif"
      Moulvoudaye -> "Moulvoudaye"
      Moutourwa -> "Moutourwa"
      Touloum -> "Touloum"
      -- MayoSava
      Kolofata -> "Kolofata"
      Mora -> "Mora"
      Tokombere -> "Tokombere"
      -- MayoTsanaga
      Bourrha -> "Bourrha"
      Hina -> "Hina"
      Koza -> "Koza"
      Mogode -> "Mogode"
      Mokolo -> "Mokolo"
      Mozogo -> "Mozogo"
      SouledeRoua -> "Soulede Roua"
      -- Littoral
      -- Moungo
      BareBakem -> "Bare Bakem"
      Bonalea -> "Bonalea"
      Dibombari -> "Dibombari"
      Loum -> "Loum"
      Manjo -> "Manjo"
      Mbanga -> "Mbanga"
      Melong -> "Melong"
      Mombo -> "Mombo"
      NjombePenjaOrPenja -> "Njombe Penja / Penja"
      NkongsambaIer -> "Nkongsamba Ier"
      NkongsambaIIe -> "Nkongsamba IIe"
      NkongsambaIIIe -> "Nkongsamba IIIe"
      NlonakoOrEbone -> "Nlonako / Ebone"
      -- Nkam
      Nkondjock -> "Nkondjock"
      NordMakombeOrNdobian -> "Nord Makombe / Ndobian"
      Yabassi -> "Yabassi"
      Yingui -> "Yingui"
      -- SanagaMaritime
      Dibamba -> "Dibamba"
      Dizangue -> "Dizangue"
      EdeaIer -> "Edéa Ier"
      EdeaIIe -> "Edéa IIe"
      MassockSongloulou -> "Massock Songloulou"
      Mouanko -> "Mouanko"
      Ndom -> "Ndom"
      Ngambe -> "Ngambe"
      Ngwei -> "Ngwei"
      Nyanon -> "Nyanon"
      Pouma -> "Pouma"
      -- Wouri
      DoualaIer -> "Douala Ier"
      DoualaIIe -> "Douala IIe"
      DoualaIIIe -> "Douala IIIe"
      DoualaIVe -> "Douala IVe"
      DoualaVe -> "Douala Ve"
      DoualaVIe -> "Douala VIe"
      Manoka -> "Manoka"
      -- North
      -- Benoue
      Bibemi -> "Bibemi"
      Dembo -> "Dembo"
      GarouaIer -> "Garoua Ier"
      GarouaIIe -> "Garoua IIe"
      GarouaIIIe -> "Garoua IIIe"
      Lagdo -> "Lagdo"
      MayoHourna -> "Mayo Hourna"
      Pitoa -> "Pitoa"
      Tcheboa -> "Tcheboa"
      Demsa -> "Demsa"
      Bascheo -> "Bascheo"
      -- Faro
      Beka -> "Beka"
      Poli -> "Poli"
      -- MayoLouti
      Figuil -> "Figuil"
      Guider -> "Guider"
      MayoOulo -> "Mayo Oulo"
      -- Mayo Rey
      Madingring -> "Madingring"
      ReyBouba -> "Rey Bouba"
      Tchollire -> "Tchollire"
      Touboro -> "Touboro"
      --  NorthWest
      -- Boyo
      Belo -> "Belo"
      Fonfuka -> "Fonfuka"
      Fundong -> "Fundong"
      Njinikom -> "Njinikom"
      -- Bui
      Jakiri -> "Jakiri"
      Kumbo -> "Kumbo"
      MbvenOrMbiame -> "Mbven / Mbiame"
      Nkum -> "Nkum"
      NoniOrNkor -> "Noni / Nkor"
      OkuOrElakOku -> "Oku / Elak Oku"
      -- DongaMantung
      Ako -> "Ako"
      Misaje -> "Misaje"
      Ndu -> "Ndu"
      Nkambe -> "Nkambe"
      Nwa -> "Nwa"
      -- Menchum
      Benakuma -> "Benakuma"
      FuruAwa -> "Furu Awa"
      Wum -> "Wum"
      Zhoa -> "Zhoa"
      -- Mezam
      Bafut -> "Bafut"
      Bali -> "Bali"
      BamendaIer -> "Bamenda Ier"
      BamendaIIe -> "Bamenda IIe"
      BamendaIIIe -> "Bamenda IIIe"
      Santa -> "Santa"
      Tubah -> "Tubah"
      -- Momo
      Andek -> "Andek"
      Batibo -> "Batibo"
      Mbengwi -> "Mbengwi"
      Njikwa -> "Njikwa"
      WidikumBoffe -> "Widikum Boffe"
      -- Ngoketunjia
      Babessi -> "Babessi"
      Balikumbat -> "Balikumbat"
      Ndop -> "Ndop"
      -- South
      -- DjaEtLobo
      Bengbis -> "Bengbis"
      Djoum -> "Djoum"
      Meyomessala -> "Meyomessala"
      Meyomessi -> "Meyomessi"
      Mintom -> "Mintom"
      Oveng -> "Oveng"
      Sangmelima -> "Sangmelima"
      Zoetele -> "Zoetele"
      -- Mvila
      BiwongBane -> "Biwong Bane"
      BiwongBulu -> "Biwong Bulu"
      EbolowaIer -> "Ebolowa Ier"
      EbolowaIIe -> "Ebolowa IIe"
      Efoulan -> "Efoulan"
      Mengong -> "Mengong"
      Mvangan -> "Mvangan"
      Ngoulemakong -> "Ngoulemakong"
      -- Ocean
      AkomII -> "Akom II"
      Bipindi -> "Bipindi"
      Campo -> "Campo"
      KribiIer -> "Kribi Ier"
      KribiIIe -> "Kribi IIe"
      Lolodorf -> "Lolodorf"
      Mvengue -> "Mvengue"
      Niete -> "Niete"
      -- ValleeDuNtem
      Ambam -> "Ambam"
      KyeOssi -> "Kye Ossi"
      Maan -> "Maan" -- ??
      Olamze -> "Olamze"
      -- SouthWest
      -- Fako
      Buea -> "Buéa"
      LimbeIer -> "Limbé Ier"
      LimbeIIe -> "Limbé IIe"
      LimbeIIIe -> "Limbé IIIe"
      Muyuka -> "Muyuka"
      Tiko -> "Tiko"
      WestCoast -> "West Coast"
      -- KoupeManengouba
      Bangem -> "Bangem"
      Nguti -> "Nguti"
      Tombel -> "Tombel"
      -- Lebialem
      Alou -> "Alou"
      Fontem -> "Fontem"
      Wabane -> "Wabane"
      -- Manyu
      Akwaya -> "Akwaya"
      Eyumodjock -> "Eyumodjock"
      Mamfe -> "Mamfe"
      TintoOrUpperBayang -> "Tinto / Upper / Bayang"
      -- Meme
      Konye -> "Konye"
      KumbaIer -> "Kumba Ier"
      KumbaIIe -> "Kumba IIe"
      KumbaIIIe -> "Kumba IIIe"
      Mbonge -> "Mbonge"
      -- Ndian
      Bamusso -> "Bamusso"
      EkondoTiti -> "Ekondo Titi"
      Idabato -> "Idabato"
      Isanguele -> "Isanguele"
      KomboAbedimo -> "Kombo Abedimo"
      KomboItindi -> "Kombo Itindi"
      Mundemba -> "Mundemba"
      -- West
      -- Bamboutos
      Babadjou -> "Babadjou"
      Batcham -> "Batcham"
      Galim -> "Galim"
      Mbouda -> "Mbouda"
      -- Haut Nkam
      Bafang -> "Bafang"
      Bakou -> "Bakou"
      Bana -> "Bana"
      Bandja -> "Bandja"
      Banka -> "Banka"
      Kekem -> "Kekem"
      -- Hauts Plateaux
      Baham -> "Baham"
      Bamendjou -> "Bamendjou"
      Bangou -> "Bangou"
      Batie -> "Batie"
      Bangam -> "Bangam"
      Badenkop -> "Badenkop"
      -- KoungKhi
      Bayangam -> "Bayangam"
      Demdeng -> "Demdeng"
      Poumougne -> "Poumougne"
      -- Menoua
      Dschang -> "Dschang"
      Fokoue -> "Fokoué"
      FongoTongo -> "Fongo Tongo"
      NkongZem -> "Nkong Zem"
      PenkaMichel -> "Penka Michel"
      Santchou -> "Santchou"
      -- Mifi
      BafoussamIer -> "Bafoussam Ier"
      BafoussamIIe -> "Bafoussam IIe"
      BafoussamIIIe -> "Bafoussam IIIe"
      -- Nde
      Bangangte -> "Bangangte"
      Bassamba -> "Bassamba"
      Bazou -> "Bazou"
      Tonga -> "Tonga"
      -- Noun
      Bangourain -> "Bangourain"
      Foumban -> "Foumban"
      Foumbot -> "Foumbot"
      Kouoptamo -> "Kouoptamo"
      Koutaba -> "Koutaba"
      Magba -> "Magba"
      Malentouen -> "Malentouen"
      Massangam -> "Massangam"
      Njimom -> "Njimom"



-- ===============================================================
-- Attribute-related types
-- ===============================================================



data Attribute
  = Attribute
      { attributeCode :: AttributeCode,
        attributeName :: AttributeName,
        attributeDescription :: ShortDescription,
        attributeValue :: Maybe AttributeValue,
        attributeUnit :: Maybe AttributeUnit
      }
  deriving (Eq, Ord, Show)

data AttributeRef
  = AttributeRef
      { attributeRefCode :: AttributeCode,
        attributeRefName :: AttributeName,
        attributeRefDescription :: ShortDescription,
        attributeRefValues :: [AttributeValue],
        attributeRefUnits :: [AttributeUnit],
        attributeRefRelatedCategories :: [(CategoryId, CategoryCode)]
      }
  deriving (Eq, Ord, Show)



-- ===============================================================
-- Owner / ContactInformation - related types
-- ===============================================================



data Person
  = Person
      { personId :: UserId,
        personContact :: ContactInformation,
        personFullName :: FullName
      }
  deriving (Eq, Ord, Show)

data ContactInformation
  = ContactInformation
      { conatInfoAddress :: Maybe PostalAddress,
        contactInfoMethod :: ContactMethod
      }
  deriving (Eq, Ord, Show)

data BothContactInfo
  = BothContactInfo
      { bothContactInfoEmail :: EmailAddress,
        bothContactInfoPrimTel :: Telephone,
        bothContactInfoSndTel :: Maybe Telephone
      }
  deriving (Eq, Ord, Show)

data ContactMethod
  = EmailOnly EmailAddress
  | PhoneOnly Telephone (Maybe Telephone)
  | EmailAndPhone BothContactInfo
  deriving (Eq, Ord, Show)

data FullName
  = FullName
      { firstName :: FirstName,
        middleName :: Maybe Middle,
        lastName :: LastName
      }
  deriving (Eq, Ord, Show)



-- ===============================================================
-- Lost Item Keeper-related types
-- ===============================================================



data Tenant
  = Tenant
      { tenantId :: TenantId,
        tenantName :: TenantName,
        tenantDescription :: LongDescription,
        tenantContactAddress :: ContactInformation
      }
  deriving (Eq, Show, Ord)



-- ===============================================================
-- Challenge-related types
-- ===============================================================



data Challenge
  = Question
  deriving (Eq, Show, Ord)

newtype ChallengeAnser
  = ChallengeAnser (Question, Answer)
  deriving (Eq, Show, Ord)



-- ===============================================================
-- Helper functions
-- ===============================================================



toRegion :: String -> Either ErrorMessage Region
toRegion str
  | "adamaoua" == lowerStr = Right Adamaoua
  | "centre" == lowerStr = Right Centre
  | "est" == lowerStr = Right East
  | "farnorth" == lowerStr = Right FarNorth
  | "littoral" == lowerStr = Right Littoral
  | "north" == lowerStr = Right North
  | "northwest" == lowerStr = Right NorthWest
  | "south" == lowerStr = Right South
  | "southwest" == lowerStr = Right SouthWest
  | "west" == lowerStr = Right West
  | otherwise = Left $ str <> ": is an invalid region code"
  where
    lowerStr = toLower <$> str

fromRegion :: Region -> String
fromRegion region =
  case region of
    Adamaoua -> show Adamaoua
    Centre -> show Centre
    East -> show East
    FarNorth -> show FarNorth
    Littoral -> show Littoral
    North -> show North
    NorthWest -> show NorthWest
    South -> show South
    SouthWest -> show SouthWest
    West -> show West

toDivision :: String -> Either ErrorMessage Division
toDivision str
  | "djerem" == lowerStr = Right Djerem
  | "faroetdeo" == lowerStr = Right FaroEtDeo
  | "mayobanyo" == lowerStr = Right MayoBanyo
  | "mbere" == lowerStr = Right Mbere
  | "vina" == lowerStr = Right Vina
  | otherwise = Left $ str <> ": is an invalid division code"
  where
    -- TODO FINISH  ALL CASES
    lowerStr = toLower <$> str

fromDivision :: Division -> String
fromDivision division =
  case division of
    Djerem -> show Djerem
    FaroEtDeo -> show FaroEtDeo
    MayoBanyo -> show MayoBanyo
    Mbere -> show Mbere
    Vina -> show Vina
    _ -> error "NOT IMPLEMENTED YET"

toSubDivision :: String -> Either ErrorMessage SubDivision
toSubDivision str
  | "gouandal" == lowerStr = Right Gouandal
  | "tibati" == lowerStr = Right Tibati
  | otherwise = Left $ str <> ": is an invalid sub division code"
  where
    -- TODO FINISH  ALL CASES
    lowerStr = toLower <$> str

fromSubDivision :: SubDivision -> String
fromSubDivision subdivision =
  case subdivision of
    Gouandal -> show Gouandal
    Tibati -> show Tibati
    _ -> error "NOT IMPLEMENTED YET"




-- ===============================================================
-- Admin Map Data Structure
-- ===============================================================




newtype AdministrativeMap
  = AdministrativeMap [RegionItem]
  deriving (Eq, Ord, Show)

data RegionItem
  = RegionItem Region [DivisionItem]
  deriving (Eq, Ord, Show)

data DivisionItem
  = DivisionItem Division [SubDivision]
  deriving (Eq, Ord, Show)

--- Helper functions

isRegionItemRegion :: Region -> RegionItem -> Bool
isRegionItemRegion reg (RegionItem regItem _) = reg == regItem

isDivisionItemDivision :: Division -> DivisionItem -> Bool
isDivisionItemDivision div (DivisionItem divItem _) = div == divItem

camerounAdministrativeMap :: AdministrativeMap
camerounAdministrativeMap = AdministrativeMap allTenRegions
  where
    allTenRegions =
      [ adamaouaRegionItem,
        centreRegionItem,
        littoralRegion
      ]

--- Adamaoua region
---
---

adamaouaRegionItem :: RegionItem
adamaouaRegionItem = RegionItem Adamaoua adamaouaDivisions
  where
    adamaouaDivisions =
      [ djeremDivision,
        faroEtDeoDivision,
        mayoBanyoDivision,
        mbereDivision,
        vinaDivision
      ]

djeremDivision :: DivisionItem
djeremDivision = DivisionItem Djerem djeremSubDivisions
  where
    djeremSubDivisions =
      [ Gouandal,
        Tibati
      ]

faroEtDeoDivision :: DivisionItem
faroEtDeoDivision = DivisionItem FaroEtDeo faroEtDeroSubDivisions
  where
    faroEtDeroSubDivisions =
      [ GalimTignere,
        MayoBaleo,
        Tignere
      ]

mayoBanyoDivision :: DivisionItem
mayoBanyoDivision = DivisionItem MayoBanyo mayoBanyoSubDivisions
  where
    mayoBanyoSubDivisions =
      [ Bankim,
        Banyo,
        MayoDarl
      ]

mbereDivision :: DivisionItem
mbereDivision = DivisionItem Mbere mbereSubDivisions
  where
    mbereSubDivisions =
      [ Dir,
        Djohong,
        Meiganga,
        Ngaoui
      ]

vinaDivision :: DivisionItem
vinaDivision = DivisionItem Vina vinaSubDivisions
  where
    vinaSubDivisions =
      [ Belel,
        Mbe,
        Nganha,
        NgaoundereIer,
        NgaoundereIIe,
        NgaoundereIIIe,
        Nyambaka,
        Martap
      ]

--- Centre region
---
---

centreRegionItem :: RegionItem
centreRegionItem = RegionItem Centre centreDivisions
  where
    centreDivisions =
      [ hauteSanagaDivision,
        lekieDivision,
        mbamEtInoubouDivision,
        mbamEtKimDivision,
        mefouEtAfambaDivision,
        mefouEtAkonoDivision,
        mfoundiDivision,
        nyongEtKelleDivision,
        nyongEtMfoumouDivision,
        nyongEtSooDivision
      ]

hauteSanagaDivision :: DivisionItem
hauteSanagaDivision = DivisionItem HauteSanaga hauteSanagaSubDivisions
  where
    hauteSanagaSubDivisions =
      [ Bibey,
        LembeYezoum,
        Mbandjock,
        Minta
      ]

lekieDivision :: DivisionItem
lekieDivision = DivisionItem Lekie lekieSubDivisions
  where
    lekieSubDivisions =
      [ Batchenga,
        Ebebda,
        EligMfomo,
        Evodoula,
        Lobo,
        Monatele,
        Obala,
        Okola,
        Saa
      ]

mbamEtInoubouDivision :: DivisionItem
mbamEtInoubouDivision = DivisionItem MbamEtInoubou mbamEtInoubouSubDivisions
  where
    mbamEtInoubouSubDivisions =
      [ Bafia,
        Bokito,
        Deuk,
        Kiiki,
        KonYambetta,
        Makenene,
        Ndikiniméki,
        Nitoukou,
        Ombessa
      ]

mbamEtKimDivision :: DivisionItem
mbamEtKimDivision = DivisionItem MbamEtKim mbamEtKimSubDivisions
  where
    mbamEtKimSubDivisions =
      [ Mbangassina,
        NgambeTikar,
        Ngoro,
        Ntui,
        Yoko
      ]

mefouEtAfambaDivision :: DivisionItem
mefouEtAfambaDivision = DivisionItem MefouEtAfamba mefouEtAfambaSubDivisions
  where
    mefouEtAfambaSubDivisions =
      [ Afanloum,
        AssambaOrOlanguina,
        Awae,
        Edzendouan,
        Esse,
        Mfou,
        Nkolafamba,
        Soa
      ]

mefouEtAkonoDivision :: DivisionItem
mefouEtAkonoDivision = DivisionItem MefouEtAkono mefouEtAkonoSubDivisions
  where
    mefouEtAkonoSubDivisions =
      [ Akono,
        Bikok,
        Mbankomo,
        Ngoumou
      ]

mfoundiDivision :: DivisionItem
mfoundiDivision = DivisionItem Mfoundi mfoundiSubDivisions
  where
    mfoundiSubDivisions =
      [ YaoundeIOrNlongkakOrEtoudi,
        YaoundeIIOrTsinga,
        YaoundeIIIOrEfoulan,
        YaoundeOrIVKondengui,
        YaoundeVOrEssos,
        YaoundeVIOrBiyemassi,
        YaoundeVIIOrNkolbisson
      ]

nyongEtKelleDivision :: DivisionItem
nyongEtKelleDivision = DivisionItem NyongEtKelle nyongEtKelleSubDivisions
  where
    nyongEtKelleSubDivisions =
      [ Biyouha,
        Bondjock,
        BotMakak,
        Dibang,
        Eseka,
        Makak,
        Matomb,
        Messondo,
        NgogMapubi,
        Nguibassal
      ]

nyongEtMfoumouDivision :: DivisionItem
nyongEtMfoumouDivision = DivisionItem NyongEtMfoumou nyongEtMfoumouSubDivisions
  where
    nyongEtMfoumouSubDivisions =
      [ Akonolinga,
        Ayos,
        Endom,
        Mengang,
        NyakokomboOrKobdombo
      ]

nyongEtSooDivision :: DivisionItem
nyongEtSooDivision = DivisionItem NyongEtSoo nyongEtSooDivisionSubDivisions
  where
    nyongEtSooDivisionSubDivisions =
      [ Akoeman,
        Dzeng,
        Mbalmayo,
        Mengueme,
        Ngomedzap,
        Nkolmetet
      ]

--- Littoral Region
---
---

littoralRegion :: RegionItem
littoralRegion = RegionItem Littoral littoralDivisons
  where
    littoralDivisons =
      [ moungoDivision,
        nkamDivision,
        sanagaMaritimeDivision,
        wouriDivision
      ]

moungoDivision :: DivisionItem
moungoDivision = DivisionItem Moungo moungoSubDivisions
  where
    moungoSubDivisions =
      [ BareBakem,
        Bonalea,
        Dibombari,
        Loum,
        Manjo,
        Mbanga,
        Melong,
        Mombo,
        NjombePenjaOrPenja,
        NkongsambaIer,
        NkongsambaIIe,
        NkongsambaIIIe,
        NlonakoOrEbone
      ]

nkamDivision :: DivisionItem
nkamDivision = DivisionItem Nkam nkamSubDivisions
  where
    nkamSubDivisions =
      [ Nkondjock,
        NordMakombeOrNdobian,
        Yabassi,
        Yingui
      ]

sanagaMaritimeDivision :: DivisionItem
sanagaMaritimeDivision = DivisionItem SanagaMaritime sanagaMaritimeSubDivisions
  where
    sanagaMaritimeSubDivisions =
      [ Dibamba,
        Dizangue,
        EdeaIer,
        EdeaIIe,
        MassockSongloulou,
        Mouanko,
        Ndom,
        Ngambe,
        Ngwei,
        Nyanon,
        Pouma
      ]

wouriDivision :: DivisionItem
wouriDivision = DivisionItem Wouri wouriSubDivisions
  where
    wouriSubDivisions =
      [ DoualaIer,
        DoualaIIe,
        DoualaIIIe,
        DoualaIVe,
        DoualaVe,
        DoualaVIe,
        Manoka
      ]



-- =============================================================================
-- Generic helper functions
-- =============================================================================


notNull = not . null
