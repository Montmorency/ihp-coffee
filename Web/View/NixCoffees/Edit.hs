module Web.View.NixCoffees.Edit where
import Web.View.Prelude

data EditView = EditView { nixCoffee :: NixCoffee }

instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={NixCoffeesAction}>NixCoffees</a></li>
                <li class="breadcrumb-item active">Edit NixCoffee</li>
            </ol>
        </nav>
        <div class="container">
        <h1>Edit NixCoffee</h1>
        {renderForm nixCoffee}
        </div>
    |]

renderForm :: NixCoffee -> Html
renderForm nixCoffee = formFor nixCoffee [hsx|
    {textField #title}
    {textareaField #body}
    {textField #labels}
    {selectField #coffeeType coffeetypes}
    { dateField #lastDrank }
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
    
