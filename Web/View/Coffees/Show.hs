module Web.View.Coffees.Show where
import Web.View.Prelude

data ShowView = ShowView { coffee :: Coffee }

instance View ShowView ViewContext where
    html ShowView { .. } = [hsx|
        <h1>Coffee</h1>
        <div class="row thread">
            <div class="col-3 user-col">
                <a class="user-col" href="#">
                    Picture of {get #coffeeType coffee}
                </a>
            </div>
            <div class="col-9">
                <div class="text-muted thread-created-at">
                    {get #createdAt coffee |> timeAgo}
                </div>
                <h1 class="coffee-title">{get #title coffee}</h1>
                <p class="coffee-body">{get #body coffee |> renderMarkdown}</p>
            </div>
        </div>
    |]
