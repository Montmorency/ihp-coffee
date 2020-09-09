module Web.FrontController where
import IHP.RouterPrelude
import IHP.ControllerSupport
import Generated.Types
import Web.Types

-- Controller Imports
import Web.Controller.Comments
import Web.Controller.Users
import Web.Controller.Coffees
import IHP.Welcome.Controller

instance FrontController WebApplication where
    controllers = 
        [ startPage CoffeesAction
        -- Generator Marker
        , parseRoute @CommentsController
        , parseRoute @UsersController
        , parseRoute @CoffeesController
        ]

instance InitControllerContext WebApplication
