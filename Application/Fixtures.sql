

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


SET SESSION AUTHORIZATION DEFAULT;

ALTER TABLE public.admins DISABLE TRIGGER ALL;



ALTER TABLE public.admins ENABLE TRIGGER ALL;


ALTER TABLE public.coffees DISABLE TRIGGER ALL;

INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('fd805872-7c71-41b7-b892-39f1d84736be', 'The Question Mark: ?', 'All variables starting with `?` are implicit parameters that are
passed around by adding constraints to function signatures. The
RequestContext is defined as:

```haskell
data RequestContext = RequestContext
    { request :: Request
    , respond :: Respond
    , params :: [Param]
    , files :: [File LBS.ByteString]
    , vault :: (Vault.Key (Session IO String String))
    }
```

When a type signature for an IHP function has a constraint`(?requestContext::RequestContext)` you know it holds references
to the wai request, request parameters, file uploads etc.', '', '2020-09-24 14:26:40.761952+01', 'espresso', '1858-11-17');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('f8b9f3a0-84ec-46b1-aab7-3dd1d47a11f8', 'Mos Def Not Think So', 'There is a Haskell module called Data.Default which defines
a type class:
```haskell
class Default a where
...
```
IHP defines a number of Default instances appropriate for different records fields e.g. an empty string for text fields:
```
instance Default Text where
   def = ""
```
and default Truth values for Bools.
```
instance Default Bool where
    def = False
```

in IHP, to help with form validation, Records often have a `Metabag` field which can be defined with a slightly fancier default:
```
instance Default MetaBag where
    def = MetaBag { annotations = [], touchedFields = [] }
```

for the MetaBag type:

```
data MetaBag = MetaBag
  { annotations :: [(Text, Text)]
  , touchedFields :: [Text]
  } deriving (Eq, Show)
```

The defaults make it easy to populate a new record''s fields.', '', '2020-09-24 14:31:56.476449+01', 'irish_coffee', '1858-11-17');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('79e20290-1885-472b-97fd-1e3f3be2059d', 'PostGres ENUM and Select Fields.', 'In a relational database a row entry might have a field with values that are restricted to a small set of options. For instance we might have a Cafe that only serves certain Types of Coffee ( we are capitalizing names that will form the data in a computer program). For accounting purposes the Cafe wants to log each sale with a Type of Coffee value selected from an enumerated list. PostgresSQL supports just such an enumerated data type. It is called an ENUM and a table entry might look like this:

```
CREATE TYPE coffeetypes AS ENUM (''americano'', ''marochino'', ''freddo'', ''mocha'', ''flat_white'' ...);
```

We can generate this ENUM data type using the IHP Schema generator. IHP will then also take care of generating all the Haskell side type values and constructors we need:

```haskell
data Coffeetype = Americano | Latte | IrishCoffee | Cappuccino | Espresso | FlatWhite | Glace | Lungo | EspressoRomano | IcedCoffee | Marochino | Freddo | Mocha deriving (Eq, Show, Read, Enum)
```

Note that IHP will take care of capitalizing, (de)pluralising, and CamelCasing the data from the Postgres tables. It also makes this datatype an instance of a number of other data families like: `Default, FromField, ToField, InputValue`. By inheriting the properties of these classes our Data Coffeetype is now in good shape to make use of all the automatic form generation and validation operations provided by IHP.

Now having dealt with the "Model" side of things let us turn to the "View" which is what the client will interact with.  We want our View to have a nice dropdown menu of Coffeetypes. To do this we need we need to make our data type an instance of `CanSelect`:

```haskell
instance CanSelect Coffeetype where
    type SelectValue Coffeetype = Coffeetype
    selectValue value = value
    selectLabel = tshow
```

`tshow` is relies on the fields having a `Show` attribute defining the string. If you want your View to have different names from the internal representation you can write them out explicitly. All this can be found in the IHP form documentation.

The final step is adding in the hsx template code:

```haskell
formFor coffee [hsx|
...
{selectField #coffeeType coffeetypes}
{submitButton}
|]

coffeetypes :: [Coffeetype]
coffeetypes =  [Americano, Latte, IrishCoffee, Cappuccino,
                         Espresso, FlatWhite, Glace, Lungo,
                         EspressoRomano, IcedCoffee, Marochino, Freddo, Mocha]
```', 'ENUM, selectfields, Forms', '2020-09-23 21:02:55.999273+01', 'latte', '1858-11-17');


ALTER TABLE public.coffees ENABLE TRIGGER ALL;


ALTER TABLE public.users DISABLE TRIGGER ALL;



ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.comments DISABLE TRIGGER ALL;



ALTER TABLE public.comments ENABLE TRIGGER ALL;


