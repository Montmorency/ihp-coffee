module Web.View.Static.StJamesStreet where
import Web.View.Prelude
   
data StJamesStreetView = StJamesStreetView
instance View StJamesStreetView where
    html StJamesStreetView = [hsx|
                                  <div class="wrapper">
                                      <object type="image/svg+xml" data="StJamesStreet.svg"></object>
                                  </div>
                              |]
    -- html StJamesStreetView = 
    -- <object type="image/svg+xml" data="StJamesStreet.svg"></object>
    -- <iframe class="full-bleed" src="StJamesStreet.svg"></iframe> 

