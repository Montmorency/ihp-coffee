module Admin.FrontController where
import IHP.RouterPrelude
import IHP.ControllerSupport
import Generated.Types
import Admin.Types

-- Controller Imports
import Admin.Controller.Admins
import Admin.Controller.Coffees
import IHP.Welcome.Controller
import IHP.LoginSupport.Middleware
import Admin.Controller.Sessions

instance FrontController AdminApplication where
    controllers = 
        [ startPage CoffeesAction
        , parseRoute @CoffeesController
        , parseRoute @SessionsController
        , parseRoute @AdminsController
        ]

instance InitControllerContext AdminApplication where
    initContext = initAuthentication @Admin
        

