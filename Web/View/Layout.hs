module Web.View.Layout (defaultLayout, Html) where

import IHP.ViewPrelude
import IHP.Environment
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Types
import Web.Routes
import qualified IHP.FrameworkConfig as FrameworkConfig
import Config ()

import Generated.Types

defaultLayout :: Html -> Html
defaultLayout inner = H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    {metaTags}
    {stylesheets}
    {scripts}
    <title>IHP-Cafe</title>
</head>
<body>
    <div class="container">
        {renderFlashMessages}
        {inner} 
        <footer class="d-flex flex-row-reverse">
        <a href="https://ihpcloud.com/NewUser?referredBy=7dd20a19-773d-4727-a682-f10b078b3da0">
            <img src="https://ihpcloud.com/deployed-with-ihp-cloud-blue.svg" alt="Deployed with IHP Cloud" height="40px"/>
        </a>
        </footer>
    </div>
</body>
|]

stylesheets :: Html
stylesheets = do
    when isDevelopment [hsx|
        <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
        <link rel="stylesheet" href="/vendor/flatpickr.min.css"/>
        <link rel="stylesheet" href="/app.css"/>
    |]
    when isProduction [hsx|
        <link rel="stylesheet" href="/prod.css"/>
    |]

scripts :: Html
scripts = do
    when isDevelopment [hsx|
        <script id="livereload-script" src="/livereload.js"></script>
        <script src="/vendor/jquery-3.2.1.slim.min.js"></script>
        <script src="/vendor/timeago.js"></script>
        <script src="/vendor/popper.min.js"></script>
        <script src="/vendor/bootstrap.min.js"></script>
        <script src="/vendor/flatpickr.js"></script>
    |]
    when isProduction [hsx|
        <script src="/vendor/jquery-3.2.1.slim.min.js"></script>
        <script src="/vendor/timeago.js"></script>
        <script src="/vendor/popper.min.js"></script>
        <script src="/vendor/bootstrap.min.js"></script>
        <script src="/vendor/flatpickr.js"></script>
    |]
     --   <script src="/vendor/morphdom-umd.min.js"></script>
     --   <script src="/helpers.js"></script>
     --   <script src="/prod.js"></script>

metaTags :: Html
metaTags = [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <meta property="og:title" content="App"/>
    <meta property="og:type" content="website"/>
    <meta property="og:url" content="TODO"/>
    <meta property="og:description" content="TODO"/>
|]

renderPicture :: Coffee -> Html
renderPicture coffee = preEscapedToHtml ("<?xml version=\"1.0\" encoding=\"utf-8\"?>" :: Text) <> [hsx|
        <svg xmlns="http://www.w3.org/2000/svg" width="100%" height="100%" viewBox="0 0 122 122" class="coffee-picture">
            <circle id="Oval" fill="#073642" cx="61" cy="61" r="60" class="icon"></circle>
            <text x="50%" y="60%" text-anchor="middle" line-spacing="60" letter-spacing="1.481481" style="font: bold 200% sans-serif" fill="hsla(196, 13%, 80%, 1)" class="icon">{text}</text>
        </svg>
    |]
        where text = get #coffeeType coffee
