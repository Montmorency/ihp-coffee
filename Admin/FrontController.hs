module Admin.FrontController where
import IHP.RouterPrelude
import IHP.ControllerSupport
import Generated.Types
import Admin.Types

-- Controller Imports
import Admin.Controller.Coffees
import IHP.Welcome.Controller
import IHP.LoginSupport.Middleware
import Admin.Controller.Sessions

instance FrontController AdminApplication where
    controllers = 
        [ startPage WelcomeAction
        , parseRoute @CoffeesController
        , parseRoute @SessionsController
        ]

instance InitControllerContext AdminApplication where
    initContext = initAuthentication @Admin
        

