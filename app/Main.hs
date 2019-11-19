module Main where


import Common.SimpleTypes
import Common.CompoundTypes

import Workflow.DeclareLostItem.PublicTypes
import Workflow.DeclareLostItem.Implementation
import Workflow.DeclareLostItem.Handler

import Workflow.CreateCategory.Common.PublicTypes
import Workflow.CreateCategory.Root.PublicTypes
import Workflow.CreateCategory.Root.Implementation
import Workflow.CreateCategory.Root.Handler

import Inventory.System.Commands
import Inventory.System.CommandsHandler
import InventoryAPI

import Control.Monad.Except


main :: IO ()
main = startApp


-- =============================================================================
-- Sample data for testing the Declare Lost Item Workflow
-- =============================================================================

unvalidateLostItem = UnvalidatedLostItem {
        uliName = "Bag Pack"
    ,   uliCategoryId = "PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP"
    ,   uliDescription = "Ce est noir et contient des livres sur la traite négrière "
    ,   uliDateAndTimeSpan = ("2019 10 10 14 20 00", "2019 10 10 15 30 00")
    ,   ulocations = [unvalidatedLocation]
    ,   uliattributes = [unvalidatedAttribute1, unvalidatedAttribute2]
    ,   uowner = unvalidatedPerson  
}




-- -----------------------------------------------------------------------------
-- Location data
--

unvalidatedLocation = UnvalidatedLocation {
        uadminArea = unvalidatedAminitrativeArea
    ,   ucity = "Douala"
    ,   uvillage = ""
    ,   uneighborhood = "Denver, Bonamoussadi"
    ,   uloaddresses = ["Carrefour 3 boutiques", "Maison Bleu", "Rond Point Maetur"]
}

-- unvalidatedAminitrativeArea = ("adamaoua", "djerem", "gouandal")

unvalidatedAminitrativeArea = ("", "", "")






-- -----------------------------------------------------------------------------
-- Attributes data
--


unvalidatedAttribute1 = UnvalidatedAttribute {
      uattrCode = "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
    , uattrName = "Color"
    , uattrDescription = "Specifie la couleur de l'objet"
    , uattrValue = "Red"
    , uattrUnit = ""
}

unvalidatedAttribute2 = UnvalidatedAttribute {
      uattrCode = "WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW"
    , uattrName = "Weight"
    , uattrDescription = "Specifie la masse de l'objet"
    , uattrValue = "10"
    , uattrUnit = "Kg"
}



-- -----------------------------------------------------------------------------
-- Owner / Declarant data
--


unvalidatedPerson = UnvalidatedPerson {
        uuserId = "111111111111111111111111111111111111"
    ,   ucontact = unvalidatedContactInformation
    ,   ufullname = unvalidatedFullName
}

unvalidatedContactInformation = UnvalidatedContactInformation {
        uemail = "felicien.fotiomanfo@gmail.com"
    ,   uaddress = "Denver, Bonamoussadi lieu dit Maison Bleu"
    ,   uprimaryTel = "626323456"
    ,   usecondaryTel = "934565456" 
}

unvalidatedFullName = UnvalidatedFullName {
        ufirst = "Felicien"
    ,   umiddle = "FOTIO"
    ,   ulast = "MANFO"  
}

declareLostItemCmd :: LostAndFoundInventoryCmd
declareLostItemCmd = Register (Command unvalidateLostItem "11:01pm" "felicien@gmail.com")


uRootCat = UnvalidatedRootCategory {
    urootCategoryCode = "HUMAN-BEINGS"
  , urootCategoryDescription = "This category is the parent category for all sub cats related to lost human beings"
  , urootCategoryEnablement = "disabled"
  , urootCatgrRelatedsubCatgrs = ["KKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK"]  
}



createCategoryCmd :: LostAndFoundInventoryCmd
createCategoryCmd = CreateRootCategory (Command uRootCat "11:01pm" "felicien@gmail.com")




res = runExceptT $ handle createCategoryCmd