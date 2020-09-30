module Web.View.Coffees.Show where
import Web.View.Prelude

data ShowView = ShowView { coffee :: Coffee }

instance View ShowView ViewContext where
    html ShowView { .. } = [hsx|
        <nav aria-label="breadcrumb">
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={StJamesStreetAction}>St. James's Street</a></li>
                <li class="breadcrumb-item active" aria-current="page">IHP Cafe</li>
            </ol>
        </nav>
        <div class="row thread">
            <div class="col-3 coffee-col">
                  <img src={(tshow (get #coffeeType coffee)) <> ".svg"}/>
            </div>
            <div class="col-9">
                <div class="text-muted thread-created-at">
                    {get #lastDrank coffee}
                </div>
                <h1 class="coffee-title">{get #title coffee}</h1>
                <p class="coffee-body">{get #body coffee |> renderMarkdown}</p>
            </div>
        </div>
    |]

-- <object data= {(tshow (get #coffeeType coffee)) <> ".svg"} type="image/svg+xml"> </object>
