module Admin.FrontController where
import Admin.Controller.Prelude
import Admin.Types
import Admin.View.Layout (defaultLayout)

-- Controller Imports
import Admin.Controller.Prelude
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
    initContext = do
        setLayout defaultLayout 
        initAuthentication @Admin

        

