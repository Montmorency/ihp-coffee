In a relational database an entry might have a field with values that are restricted to a small set of options. For instance we might have a Cafe that only serves certain types of coffee. For accounting purposes they want to log each sale with a coffee type value selected from this enumerated list. The PSQL supports such a data type called an ENUM and a table entry might look like this:

```
CREATE TYPE coffeetypes AS ENUM ('americano', 'marochino', 'freddo', 'mocha' ...);
```

We can generate this table using the IHP Schema generator. IHP will then also take care of generating all the Haskell types we need:

```
data Coffeetype = Americano | Latte | IrishCoffee | Cappuccino | Espresso | FlatWhite | Glace | Lungo | EspressoRomano | IcedCoffee | Marochino | Freddo | Mocha deriving (Eq, Show, Read, Enum)
```

IHP even takes care of capitalizing and (de)pluralising the datatypes from the PSQL tablename. It also makes this datatype an instance of a number of other data families like: `Default, FromField, ToField, InputValue`. By inheriting the properties of these classes our Data Coffeetype is now in good shape to make use of all the automatic form generation and validation operations provided by IHP. 

Say on the client side we want our view to have a nice dropdown menu of coffeetypes. To do this we need we need to make our data type an instance of `CanSelect`:

```
instance CanSelect Coffeetype where
    type SelectValue Coffeetype = Coffeetype
    selectValue value = value
    selectLabel = tshow
```

`tshow` is a function that saves us from setting all the individual
label names. If you want your View to have different names from the internal representation
you can write them out explicitly. All this can be found in the IHP form documentation.

The final step is adding in the hsx template code:

```
formFor coffee [hsx|
...
{selectField #coffeeType coffeetypes}
{submitButton}
|]

coffeetypes :: [Coffeetype]
coffeetypes =  [Americano, Latte, IrishCoffee, Cappuccino,
                         Espresso, FlatWhite, Glace, Lungo,
                         EspressoRomano, IcedCoffee, Marochino, Freddo, Mocha]
```

