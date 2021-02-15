module Web.View.Static.StJamesStreet where
import Web.View.Prelude
   
data StJamesStreetView = StJamesStreetView
instance View StJamesStreetView where
    html StJamesStreetView = [hsx|
                                  <div class="wrapper">
                                    <iframe class="full-bleed" src="StJamesStreet.svg"></iframe> 
                                  </div>
                              |]
    --html StJamesStreetView = [hsx|<object type="image/svg+xml" data="StJamesStreet.svg"></object>|]

