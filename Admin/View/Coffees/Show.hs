module Admin.View.Coffees.Show where
import Admin.View.Prelude

data ShowView = ShowView { coffee :: Coffee }

instance View ShowView ViewContext where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={CoffeesAction}>Coffees</a></li>
                <li class="breadcrumb-item active">Show Coffee</li>
            </ol>
        </nav>
        <h1>Show Coffee</h1>
    |]
