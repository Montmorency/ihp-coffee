module Admin.View.NixCoffees.Index where
import Admin.View.Prelude

data IndexView = IndexView { nixCoffees :: [NixCoffee] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={NixCoffeesAction}>NixCoffees</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewNixCoffeeAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="coffees"> {forEach nixCoffees renderCoffee} </div>
    |]

renderCoffee coffee = [hsx|
    <div class="row coffee">
        <div class="col-3">
        <span class="ml-1 text-muted"> {get #coffeeType coffee} </span>
        </div>
        <div class="col-6">
        <a class="coffee-title" href={ShowNixCoffeeAction (get #id coffee)}> {get #title coffee} </a>
        </div>
        <div class="col-1">
        <a class="coffee-action-link" href={EditNixCoffeeAction (get #id coffee)}> Edit </a>
        </div>
        <div class="col-1">
        <a class="coffee-action-link" href={DeleteNixCoffeeAction (get #id coffee)}> Delete </a>
        </div>
    </div>
|]
