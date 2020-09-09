module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types

-- Generator Marker
instance AutoRoute CoffeesController
type instance ModelControllerMap WebApplication Coffee = CoffeesController

instance AutoRoute UsersController
type instance ModelControllerMap WebApplication User = UsersController

instance AutoRoute CommentsController
type instance ModelControllerMap WebApplication Comment = CommentsController

