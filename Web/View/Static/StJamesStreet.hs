module Web.View.Static.StJamesStreet where
import Web.View.Prelude
   
data StJamesStreetView = StJamesStreetView
instance View StJamesStreetView ViewContext where
    html StJamesStreetView = [hsx|"Don't need this atm."|]
