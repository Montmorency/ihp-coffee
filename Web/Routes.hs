module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types

-- Generator Marker
instance AutoRoute CoffeesController

instance AutoRoute UsersController

instance AutoRoute CommentsController

instance AutoRoute StaticController

type instance ModelControllerMap WebApplication Coffee = CoffeesController

type instance ModelControllerMap WebApplication User = UsersController

type instance ModelControllerMap WebApplication Comment = CommentsController

