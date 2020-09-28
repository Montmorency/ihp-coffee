module Admin.View.Coffees.Show where
import Admin.View.Prelude
import Web.Controller.Static

data ShowView = ShowView { coffee :: Coffee }

instance View ShowView ViewContext where
    html ShowView { .. } = [hsx|
        <nav aria-label="breadcrumb">
            <ol class="breadcrumb">
                <li class="breadcrumb-item active" aria-current="page">IHP Cafe</li>
            </ol>
        </nav>
        <div class="row thread">
            <div class="col-3 coffee-col">
                <img src={(tshow (get #coffeeType coffee))++".svg"} alt={tshow (get #coffeeType coffee)} width="100%" height="100%">
            </div>
            <div class="col-9">
                <div class="text-muted thread-created-at">
                    {get #lastDrank coffee}
                </div>
                <h1 class="coffee-title">{get #title coffee}</h1>
                <p class="coffee-body">{get #body coffee |> renderMarkdown}</p>
            </div>
        </div>
    <footer style="margin-top: 3rem; background-color: #073642; padding-top: 2rem; padding-bottom: 2rem; color:hsla(196, 13%, 96%, 1)">
        <div class="container">
            <a href="https://ihpcloud.com/NewUser?referredBy=7dd20a19-773d-4727-a682-f10b078b3da0">Join the IHP Cloud!</a>
        </div>
    </footer>
    |]
