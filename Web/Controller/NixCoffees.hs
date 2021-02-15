module Web.Controller.NixCoffees where

import Web.Controller.Prelude
import Web.View.NixCoffees.Show

instance Controller NixCoffeesController where

    action TodaysNixCoffeeAction  = do
        nixCoffee <- query @ NixCoffee
            |> orderBy #lastDrank
            |> fetchOne
        nixCoffees <- query @NixCoffee |> fetch

        render ShowView { .. }

    action ShowNixCoffeeAction { nixCoffeeId } = do
        nixCoffee <- fetch nixCoffeeId
        nixCoffees <- query @NixCoffee |> fetch
        render ShowView { .. }

buildNixCoffee nixCoffee = nixCoffee
    |> fill @["title","body","labels","lastDrank","coffeeType"]
