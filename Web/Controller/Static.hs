module Web.Controller.Static where

import Web.Controller.Prelude
import Web.View.Static.StJamesStreet

instance Controller StaticController where
    --action StJamesStreetAction = renderFile "static/StJamesStreet.svg" "image/svg+xml"
    action StJamesStreetAction = render StJamesStreetView
