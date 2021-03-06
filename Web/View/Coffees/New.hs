module Web.View.Coffees.New where
import Web.View.Prelude --exports Generated.Types
import Data.Data

data NewView = NewView { coffee :: Coffee }

instance View NewView  where
    html NewView { .. } = [hsx|
        <h1>New Coffee</h1>
        {renderForm coffee}
    |]

renderForm :: Coffee -> Html
renderForm coffee = formFor coffee [hsx|
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
    
