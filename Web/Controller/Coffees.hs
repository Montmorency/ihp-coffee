module Web.Controller.Coffees where

import Web.Controller.Prelude
import Web.View.Coffees.Index
import Web.View.Coffees.New
import Web.View.Coffees.Edit
import Web.View.Coffees.Show

instance Controller CoffeesController where
    action CoffeesAction = do
        coffees <- query @Coffee 
            |> fetch
        render IndexView { .. }

    action TodaysCoffeeAction  = do
        coffee <- query @ Coffee
            |> orderBy #lastDrank
            |> fetchOne
        render ShowView { .. }

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
        --ensureIsBarista haha 
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

buildCoffee coffee = coffee
    |> fill @["title","body","labels","coffeeType"]
