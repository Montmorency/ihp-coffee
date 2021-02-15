module Admin.Controller.NixCoffees where

import Admin.Controller.Prelude
import Admin.View.NixCoffees.Index
import Admin.View.NixCoffees.New
import Admin.View.NixCoffees.Edit
import Admin.View.NixCoffees.Show

instance Controller NixCoffeesController where
    beforeAction = ensureIsAdmin @Admin

    action NixCoffeesAction = do
        nixCoffees <- query @NixCoffee |> fetch
        render IndexView { .. }

    action TodaysNixCoffeeAction  = do
        nixCoffee <- query @ NixCoffee
            |> orderBy #lastDrank
            |> fetchOne

        nixCoffees <- query @NixCoffee |> fetch

        render ShowView { .. }

    action NewNixCoffeeAction = do
        let nixCoffee = newRecord
        render NewView { .. }

    action ShowNixCoffeeAction { nixCoffeeId } = do
        nixCoffee <- fetch nixCoffeeId
        nixCoffees <- query @NixCoffee |> fetch

        render ShowView { .. }

    action EditNixCoffeeAction { nixCoffeeId } = do
        nixCoffee <- fetch nixCoffeeId
        render EditView { .. }

    action UpdateNixCoffeeAction { nixCoffeeId } = do
        nixCoffee <- fetch nixCoffeeId
        nixCoffee
            |> buildNixCoffee
            |> ifValid \case
                Left nixCoffee -> render EditView { .. }
                Right nixCoffee -> do
                    nixCoffee <- nixCoffee |> updateRecord
                    setSuccessMessage "NixCoffee updated"
                    redirectTo EditNixCoffeeAction { .. }

    action CreateNixCoffeeAction = do
        let nixCoffee = newRecord @NixCoffee
        nixCoffee
            |> buildNixCoffee
            |> ifValid \case
                Left nixCoffee -> render NewView { .. } 
                Right nixCoffee -> do
                    nixCoffee <- nixCoffee |> createRecord
                    setSuccessMessage "NixCoffee created"
                    redirectTo NixCoffeesAction

    action DeleteNixCoffeeAction { nixCoffeeId } = do
        nixCoffee <- fetch nixCoffeeId
        deleteRecord nixCoffee
        setSuccessMessage "NixCoffee deleted"
        redirectTo NixCoffeesAction

buildNixCoffee nixCoffee = nixCoffee
    |> fill @["title","body","labels","lastDrank","coffeeType"]
