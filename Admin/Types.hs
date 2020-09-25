module Admin.Types where
import IHP.Prelude
import qualified IHP.Controller.Session
import qualified IHP.ControllerSupport as ControllerSupport
import IHP.ModelSupport
import Application.Helper.Controller
import IHP.ViewSupport
import Generated.Types

import IHP.LoginSupport.Types


data AdminApplication = AdminApplication deriving (Eq, Show)

data ViewContext = ViewContext
    { requestContext :: ControllerSupport.RequestContext
    , admin :: Maybe Admin
    , flashMessages :: [IHP.Controller.Session.FlashMessage]
    , controllerContext :: ControllerSupport.ControllerContext
    , layout :: Layout
    }

data CoffeesController
    = CoffeesAction
    | NewCoffeeAction
    | ShowCoffeeAction { coffeeId :: !(Id Coffee) }
    | CreateCoffeeAction
    | EditCoffeeAction { coffeeId :: !(Id Coffee) }
    | UpdateCoffeeAction { coffeeId :: !(Id Coffee) }
    | DeleteCoffeeAction { coffeeId :: !(Id Coffee) }
    deriving (Eq, Show, Data)

data SessionsController
    = NewSessionAction | CreateSessionAction | DeleteSessionAction   
     deriving (Eq, Show, Data)

instance HasNewSessionUrl Admin where
    newSessionUrl _ = "/NewSession"
