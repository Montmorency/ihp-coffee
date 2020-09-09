module Web.View.Coffees.Show where
import Web.View.Prelude

data ShowView = ShowView { coffee :: Coffee }

instance View ShowView ViewContext where
    html ShowView { .. } = [hsx|
        <h1>Coffee</h1>
    |]
