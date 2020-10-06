module Web.View.Users.ShowPlaceholderPicture where 

import Web.View.Prelude
import qualified Data.Text as Text

data ShowPlaceholderPicture = ShowPlaceholderPicture { user :: User } deriving (Typeable)

instance View ShowPlaceholderPicture ViewContext where
    beforeRender (context, view) = (context { layout = \v -> v }, view)
    html ShowPlaceholderPicture { .. } = preEscapedToHtml ("<?xml version=\"1.0\" encoding=\"utf-8\"?>" :: Text) <> [hsx|
        <svg xmlns="http://www.w3.org/2000/svg" width="100%" height="100%" viewBox="0 0 122 122" class="user-icon">
            <circle id="Oval" fill="hsla(207, 90%, 89%, 1)" cx="61" cy="61" r="60" class="icon"></circle>
            <text x="50%" y="60%" text-anchor="middle" line-spacing="60" letter-spacing="1.481481" style="font: bold 250% sans-serif" fill="#2196f3" class="icon">{text}</text>
        </svg>
    |]
