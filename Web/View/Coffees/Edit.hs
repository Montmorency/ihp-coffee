module Web.View.Coffees.Edit where
import Web.View.Prelude

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
    {textField #body}
    {textField #labels}
    {submitButton}
|]
