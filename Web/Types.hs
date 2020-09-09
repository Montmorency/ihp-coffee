module Web.Types where
import IHP.Prelude
import qualified IHP.Controller.Session
import qualified IHP.ControllerSupport as ControllerSupport
import IHP.ModelSupport
import Application.Helper.Controller
import IHP.ViewSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)

data ViewContext = ViewContext
    { requestContext :: ControllerSupport.RequestContext
    , flashMessages :: [IHP.Controller.Session.FlashMessage]
    , controllerContext :: ControllerSupport.ControllerContext
    , layout :: Layout
    }

data CoffeesController
    = CoffeesAction
    | NewCoffeeAction
    | ShowCoffeeAction { coffeeId :: !(Id Coffee) }
    | ShowTodaysCoffeeAction { coffeeId :: !(Id Coffee) }
    | CreateCoffeeAction
    | EditCoffeeAction { coffeeId :: !(Id Coffee) }
    | UpdateCoffeeAction { coffeeId :: !(Id Coffee) }
    | DeleteCoffeeAction { coffeeId :: !(Id Coffee) }
    deriving (Eq, Show, Data)

data UsersController
    = UsersAction
    | NewUserAction
    | ShowUserAction { userId :: !(Id User) }
    | CreateUserAction
    | EditUserAction { userId :: !(Id User) }
    | UpdateUserAction { userId :: !(Id User) }
    | DeleteUserAction { userId :: !(Id User) }
    deriving (Eq, Show, Data)

data CommentsController
    = CommentsAction
    | NewCommentAction
    | ShowCommentAction { commentId :: !(Id Comment) }
    | CreateCommentAction
    | EditCommentAction { commentId :: !(Id Comment) }
    | UpdateCommentAction { commentId :: !(Id Comment) }
    | DeleteCommentAction { commentId :: !(Id Comment) }
    deriving (Eq, Show, Data)
