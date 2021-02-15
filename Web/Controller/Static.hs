module Web.Controller.Static where

import Web.Controller.Prelude
import Web.View.Static.StJamesStreet

import Network.Wai (responseLBS)
import qualified Data.ByteString.Char8 as B8
import qualified Network.HTTP.Types.Status as HTS
import Network.HTTP.Types.Header

forceRedirectTo path = respondAndExit $ responseLBS (HTS.Status 280 (B8.pack "IHP Redirect")) [(hLocation, cs (fromConfig baseUrl <> path))] ""

instance Controller StaticController where
    action StJamesStreetAction = render StJamesStreetView

    action RefreshStreetView = forceRedirectTo ( pathTo StJamesStreetAction )
    --action StJamesStreetAction = renderFile "static/StJamesStreet.svg" "image/svg+xml"
