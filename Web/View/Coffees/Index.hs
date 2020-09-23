module Web.View.Coffees.Index where
import Web.View.Prelude

data IndexView = IndexView { coffees :: [Coffee] }

instance View IndexView ViewContext where
    html IndexView { .. } = [hsx|
        <h1>Coffees <a href={pathTo NewCoffeeAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="coffees">{forEach coffees renderCoffee}</div>
    |]

renderCoffee coffee = [hsx|
    <div class="row coffee">
        <div class="col-3">
        <span class="ml-1 text-muted"> {get #coffeeType coffee} </span>
        </div>
        <div class="col-6">
        <a class="coffee-title" href={ShowCoffeeAction (get #id coffee)}> {get #title coffee} </a>
        </div>
        <div class="col-1">
        <a class="coffee-action-link" href={EditCoffeeAction (get #id coffee)}> Edit </a>
        </div>
        <div class="col-1">
        <a class="coffee-action-link" href={DeleteCoffeeAction (get #id coffee)}> Delete </a>
        </div>
    </div>
|]
