module Web.FrontController where
import IHP.RouterPrelude
import IHP.ControllerSupport
import Generated.Types
import Web.Types

-- Controller Imports
import Web.Controller.Comments
import Web.Controller.Users
import Web.Controller.Coffees
import Web.Controller.Static

import IHP.Welcome.Controller
import IHP.LoginSupport.Middleware

instance FrontController WebApplication where
    controllers = 
        [ startPage TodaysCoffeeAction
        , parseRoute @CoffeesController
        , parseRoute @StaticController
        ]

instance InitControllerContext WebApplication
