module Admin.View.NixCoffees.New where
import Admin.View.Prelude

data NewView = NewView { nixCoffee :: NixCoffee }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={NixCoffeesAction}>NixCoffees</a></li>
                <li class="breadcrumb-item active">New NixCoffee</li>
            </ol>
        </nav>
        <div class="container">
        <h1>New NixCoffee</h1>
        {renderForm nixCoffee}
        </div>
    |]

renderForm :: NixCoffee -> Html
renderForm nixCoffee = formFor nixCoffee [hsx|
    { textField #title }
    { (textareaField #body) {helpText = "(markdown enabled.)"} }
    { selectField #coffeeType coffeetypes }
    { dateField #lastDrank }
    { submitButton }
|]
    where 
        coffeetypes :: [Coffeetype]
        coffeetypes =  [Americano, Latte, IrishCoffee, Cappuccino, 
                        Espresso, FlatWhite, Glace, Lungo, 
                        EspressoRomano, IcedCoffee, Marochino, Freddo, Mocha]

instance CanSelect Coffeetype where
    type SelectValue Coffeetype = Coffeetype
    selectValue value = value 
    selectLabel = tshow
    
