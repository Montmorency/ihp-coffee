module Admin.Routes where
import IHP.RouterPrelude
import Generated.Types
import Admin.Types

-- Generator Marker
instance AutoRoute CoffeesController
instance AutoRoute SessionsController

type instance ModelControllerMap AdminApplication Coffee = CoffeesController

