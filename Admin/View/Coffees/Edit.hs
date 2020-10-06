module Admin.View.Coffees.Edit where
import Admin.View.Prelude

data EditView = EditView { coffee :: Coffee }

instance View EditView ViewContext where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={CoffeesAction}>Coffees</a></li>
                <li class="breadcrumb-item active">Edit Coffee</li>
            </ol>
        </nav>
        <h1>Edit Coffee</h1>
        {renderForm coffee}
    |]

renderForm :: Coffee -> Html
renderForm coffee = formFor coffee [hsx|
    {textField #title}
    {textareaField #body}
    {textField #labels}
    {selectField #coffeeType coffeetypes}
    {dateField #createdAt}
    {dateField #lastDrank}
    {submitButton}
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
    
