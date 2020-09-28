module Application.Helper.View (
    module IHP.LoginSupport.Helper.View, renderMarkdown,
           currentAdmin, currentAdminOrNothing
) where

-- Here you can add functions which are available in all your views
-- To use the built in login:
import IHP.Prelude
import IHP.ViewPrelude

import IHP.LoginSupport.Helper.View

import qualified Text.MMark as MMark
import Text.MMark.Extension.GhcSyntaxHighlighter


renderMarkdown text =
    case text |> MMark.parse "" of
        Left error -> "Something went wrong"
        Right markdown ->
                markdown
                |> MMark.useExtension ghcSyntaxHighlighter
                |> MMark.render
                |> tshow
                |> preEscapedToHtml


currentAdmin :: (?viewContext :: viewContext, HasField "admin" viewContext (Maybe admin)) => admin
currentAdmin = fromMaybe (error "Application.Helper.View.currentAdmin: Not logged in") currentAdminOrNothing

currentAdminOrNothing :: (?viewContext :: viewContext, HasField "admin" viewContext (Maybe admin)) => Maybe admin
currentAdminOrNothing = getField @"admin" ?viewContext 
