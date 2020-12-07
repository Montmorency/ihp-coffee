module Web.FrontController where

import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

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

instance InitControllerContext WebApplication where
    initContext = do
            setLayout defaultLayout
