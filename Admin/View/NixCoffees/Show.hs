module Admin.View.NixCoffees.Show where
import Admin.View.Prelude
--import Web.Controller.Prelude (StJamesStreetAction)

data ShowView = ShowView { nixCoffee :: NixCoffee, nixCoffees :: [NixCoffee] }

instance View ShowView  where
    html ShowView { .. } = [hsx|
        <div class="d-flex-md coffee-content">
            <div>
                  <img src= { (tshow (get #coffeeType nixCoffee)) <> ".svg" } />
            </div>
            <div>
                <div class="text-muted thread-created-at">
                    {get #lastDrank nixCoffee}
                </div>
                <h1 class="coffee-title"> { get #title nixCoffee } </h1>
                <p class="coffee-body p-2"> { get #body nixCoffee |> renderMarkdown } </p>
            </div>
            <div>
                { forEach nixCoffees renderCoffee }
            </div>
        </div>
    |]

renderCoffee :: NixCoffee -> Html
renderCoffee nixcoffee = [hsx|
        <div class="d-block">
        <span class="ft-size-14"> <a class="nixcoffee-link" href={ShowNixCoffeeAction (get #id nixcoffee)}> {get #title nixcoffee} </a> </span>
        </div>
 |]


