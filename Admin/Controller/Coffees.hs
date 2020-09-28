module Admin.Controller.Coffees where

import Admin.Controller.Prelude

import Admin.View.Coffees.Index
import Admin.View.Coffees.New
import Admin.View.Coffees.Edit
import Admin.View.Coffees.Show
import Admin.View.Sessions.New

instance Controller CoffeesController where
    beforeAction = ensureIsAdmin @Admin

    action CoffeesAction = do
        coffees <- query @Coffee |> fetch
        render IndexView { .. }

    action NewCoffeeAction = do
        let coffee = newRecord
        render NewView { .. }

    action ShowCoffeeAction { coffeeId } = do
        coffee <- fetch coffeeId
        render ShowView { .. }

    action EditCoffeeAction { coffeeId } = do
        coffee <- fetch coffeeId
        render EditView { .. }

    action UpdateCoffeeAction { coffeeId } = do
        coffee <- fetch coffeeId
        coffee
            |> buildCoffee
            |> ifValid \case
                Left coffee -> render EditView { .. }
                Right coffee -> do
                    coffee <- coffee |> updateRecord
                    setSuccessMessage "Coffee updated"
                    redirectTo EditCoffeeAction { .. }

    action CreateCoffeeAction = do
        let coffee = newRecord @Coffee
        coffee
            |> buildCoffee
            |> ifValid \case
                Left coffee -> render NewView { .. } 
                Right coffee -> do
                    coffee <- coffee |> createRecord
                    setSuccessMessage "Coffee created"
                    redirectTo CoffeesAction

    action DeleteCoffeeAction { coffeeId } = do
        coffee <- fetch coffeeId
        deleteRecord coffee
        setSuccessMessage "Coffee deleted"
        redirectTo CoffeesAction

buildCoffee coffee = coffee
    |> fill @["title","body","labels","coffeeType","lastDrank"]
