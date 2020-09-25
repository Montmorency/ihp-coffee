module Admin.View.Coffees.Index where
import Admin.View.Prelude

data IndexView = IndexView { coffees :: [Coffee] }

instance View IndexView ViewContext where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={CoffeesAction}>Coffees</a></li>
            </ol>
            <a href={NewSessionAction}>Login</a>
        </nav>
        <h1>Index <a href={pathTo NewCoffeeAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Coffee</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach coffees renderCoffee}</tbody>
            </table>
        </div>
    |]


renderCoffee coffee = [hsx|
    <tr>
        <td>{coffee}</td>
        <td><a href={ShowCoffeeAction (get #id coffee)}>Show</a></td>
        <td><a href={EditCoffeeAction (get #id coffee)} class="text-muted">Edit</a></td>
        <td><a href={DeleteCoffeeAction (get #id coffee)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
