module Web.View.Coffees.Show where
import Web.View.Prelude

data ShowView = ShowView { coffee :: Coffee }

instance View ShowView ViewContext where
    html ShowView { .. } = [hsx|
        <nav aria-label="breadcrumb">
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={CoffeesAction}>St. James's Street</a></li>
                <li class="breadcrumb-item active" aria-current="page">IHP Cafe</li>
            </ol>
        </nav>
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
