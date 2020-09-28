module Web.Controller.Coffees where

import Web.Controller.Prelude
import Web.View.Coffees.Show

--import Web.View.Coffees.Index
--import Web.View.Coffees.New
--import Web.View.Coffees.Edit


instance Controller CoffeesController where

    action TodaysCoffeeAction  = do
        coffee <- query @ Coffee
            |> orderBy #lastDrank
            |> fetchOne
        render ShowView { .. }

    action ShowCoffeeAction { coffeeId } = do
        coffee <- fetch coffeeId
        render ShowView { .. }

{-- ONLY ADMIN/Baristas Can changes coffees 
    action CoffeesAction = do
        coffees <- query @Coffee 
            |> fetch
        render IndexView { .. }

    action NewCoffeeAction = do
        let coffee = newRecord
        render NewView { .. }

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
                Left coffee -> do
                    render NewView { .. } 
                Right coffee -> do
                    coffee <- coffee |> createRecord
                    setSuccessMessage "Coffee created"
                    let coffeeId = get #id coffee
                    redirectTo ShowCoffeeAction {coffeeId}

    action DeleteCoffeeAction { coffeeId } = do
        coffee <- fetch coffeeId
        deleteRecord coffee
        setSuccessMessage "Coffee deleted"
        redirectTo CoffeesAction
--}

buildCoffee coffee = coffee
    |> fill @["title","body","labels","coffeeType"]
