module Admin.View.Coffees.New where
import Admin.View.Prelude

data NewView = NewView { coffee :: Coffee }

instance View NewView ViewContext where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={CoffeesAction}>Coffees</a></li>
                <li class="breadcrumb-item active">New Coffee</li>
            </ol>
        </nav>
        <h1>New Coffee</h1>
        {renderForm coffee}
    |]

renderForm :: Coffee -> Html
renderForm coffee = formFor coffee [hsx|
    {textField #title}
    {textField #body}
    {textField #labels}
    {textField #coffeeType}
    {textField #lastDrank}
    {submitButton}
|]
