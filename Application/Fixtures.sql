

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

INSERT INTO public.admins (id, email, password_hash, locked_at, failed_login_attempts, name) VALUES ('d17e8e04-52ec-4539-8151-94575b3c9f79', 'lamberh@tcd.ie', 'sha256|17|QbTKQ6mw5pKRGR+NI0SW/Q==|u/sl4eJzQnmBw++OdXHivLVRkYtlfhB6LrhCZzB1fEY=', NULL, 0, 'Henry');


ALTER TABLE public.admins ENABLE TRIGGER ALL;


ALTER TABLE public.coffees DISABLE TRIGGER ALL;

INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('c7353f29-3502-4595-b56a-42462c8a3dff', 'Automatic Sessions', 'IHP gives support for handling User sessions and authentication straight out of the box. The documentation describes the details. Here we''ll just do a quick dive into what happens behind the scenes when you have an IHP Controller instance defined like this:

```haskell
instance Controller CoffeesController where 
    beforeAction = ensureIsAdmin @Admin
```
This controller logic ensures that all other actions defined in the Controller will only be available to clients that have logged in as an Administrator. 
The function is defined like this:
```haskell
ensureIsAdmin =
    case currentAdminOrNothing @admin of
        Just _ -> pure ()
        Nothing -> redirectToLoginWithMessage (newSessionUrl (Proxy :: Proxy admin))
```
If the user is logged in as Admin we have purity and the IO consumes a unit and moves on. If there is no Admin logged in the client is redirected to the newSessionUrl which is typically just a login page. The `newSessionUrl` method was defined (as you''ll see in the documentation) in when you made your `Admin` data type an instance of `HasNewSessionUrl`:

```haskell
instance HasNewSessionUrl Admin where
    newSessionUrl admin = "/NewSession"
```

The way the controller logic and implicit parameters are handled and connected to Views in IHP makes standard authorization/authentication patterns very concise for the programmer to write. ', 'Sessions', '2020-09-28 13:51:37.428757+01', 'americano', '2020-09-08');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('fd805872-7c71-41b7-b892-39f1d84736be', 'The Question Mark: ?', 'All variables starting with `?` are implicit parameters that are
passed around by adding constraints to function signatures. For example the
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
to the wai request, request parameters, file uploads etc.', 'Implicit Parameters', '2020-09-24 14:26:40.761952+01', 'espresso', '1858-11-17');
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
and default Truth values for booleans:
```
instance Default Bool where
    def = False
```

In IHP, mainly to help with form validation, Models have a `Metabag` field which can be defined with a slightly fancier default:

```
instance Default MetaBag where
    def = MetaBag { annotations = [], touchedFields = [] }
```

that is `MetaBag` records which are data types defined as:

```
data MetaBag = MetaBag
  { annotations :: [(Text, Text)]
  , touchedFields :: [Text]
  } deriving (Eq, Show)
```

Defaults make it easy to populate a new record''s data fields in the database without worrying about writing a lot of boilerplate.', 'records, models, defaults', '2020-09-24 14:31:56.476449+01', 'irish_coffee', '1858-11-17');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('79e20290-1885-472b-97fd-1e3f3be2059d', 'PostGres ENUM and Select Fields.', 'In a relational database a row entry might have a field with values that are restricted to a small set of options. For instance we might have a Cafe that only serves certain Types of Coffee ( we are capitalizing names that will form the data in a computer program). For accounting purposes the Cafe wants to log each sale with a Type of Coffee value selected from an enumerated list. PostgresSQL supports just such an enumerated data type. It is called an `ENUM` and a table entry might look like this:

```
CREATE TYPE coffeetypes AS ENUM (''americano'', ''marochino'', ''freddo'', ''mocha'', ''flat_white'' ...);
```

We can generate this `ENUM` data type using the IHP Schema generator. IHP will then also take care of generating all the Haskell side type values and constructors we need:

```haskell
data Coffeetype = Americano | Latte | IrishCoffee | Cappuccino | Espresso | FlatWhite | Glace | Lungo | EspressoRomano | IcedCoffee | Marochino | Freddo | Mocha deriving (Eq, Show, Read, Enum)
```

Note that IHP will take care of capitalizing, (de)pluralising, and CamelCasing the data from the Postgres tables. It also makes this datatype an instance of a number of other data families like: `Default, FromField, ToField, InputValue`. By inheriting the properties of these classes our `data Coffeetype` is now in good shape to make use of all the automatic form generation and validation operations provided by IHP.

Now having dealt with the "Model" side of things let us turn to the "View" which is what the client will interact with.  We want our View to have a nice dropdown menu of Coffeetypes. To do this we need to make our data type an instance of `CanSelect`:

```haskell
instance CanSelect Coffeetype where
    type SelectValue Coffeetype = Coffeetype
    selectValue value = value
    selectLabel = tshow
```

`tshow` is a helper function that relies on the fields having a `Show` attribute defining the string. If you want your "View" to have different names from the internal representation you can write them out explicitly. All this can be found in the IHP form documentation.

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
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('c82c853a-2a2f-462c-910f-e7a723077015', 'Admiral Admin', 'Learning how IHP works has the added benefit of making
some of our ideas about Haskell more concrete.
Let us look at how `LoginSupport` works in IHP.
In the process we will see an example of the subtle
difference between a `type` synonym and a `type instance` synonym.

Firstly where does the `Admin` data type come from?
It came from you! You (should have)
built that record into your Application''s Postgres Schema:
To use the builtin login support the table should have
certain IHP required fields like an `email`, `locked_at`,
and a `password_hash`.
When you defined the admins record in your Postgres Database:

```
CREATE TABLE admins (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT null NOT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL,
    name TEXT NOT NULL
);
```
IHP generated all the Haskell side types (with its clever tricks for pluralizing names,
CamelCasing data etc.). It went to the trouble of taking `TABLE admins`
and generated you an `Admin` data type.
Have a look in `./build/Generated/Types.hs` to see what `instance` "armour" is placed on `Admin`.
As you can see in `./build/Generated/Types.hs` IHP equipped
your data to the teeth with useful instances
for going and doing battle on the internet.

Now in `Application/Helper/Controller.hs` we should have defined something like
`type instance CurrentAdminRecord = Admin`. In other words we make a type instance
synonym called `CurrentAdminRecord` for our Admin data type. It is a type instance
declaration because it a synonym for a `type family CurrentAdminRecord`
(defined in `LoginSupport/Helper/Controller.hs`)
rather than the straightforward `type Age = Int` synonym for
simple data types. A `type family` plays a similar role for data that
a `type class` plays for polymorphic functions.

Now that the Admin data type is in place there is some configuration that needs
to be done so that the session will keep track of whether or not an Admin
is logged in. The IHP documentation will set you on the right path here. There
is a predefined Controller which takes care of creating and deleting sessions
and redirecting the client to a login page.

This being done. You can now stick the function `ensureIsAdmin`
at the head of your Controller action logic
to make sure that only Administrators can do important Admin things:

```haskell
instance Controller CoffeesController where
   beforeAction = ensureIsAdmin @Admin

   action CoffeesAction = do
       coffee <- blendYourselfACoffee
       ...
```

The `beforeAction` method will ensure any subsequent action in the Controller
is coming from a valid Administrator and, if it doesn''t, it will redirect to the Admin login
page if it is not.

These kind of patterns make factoring permission based Views in your IHP application
simple to implement and secure.', 'Admin, Sessions', '2020-09-30 10:45:45.176187+01', 'freddo', '1858-11-18');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('e22b478c-0ae4-41d0-85bd-577ac2932d7d', 'Filling Forms MetaFormically ', 'The `fill` function takes care of populating multiple fields in a record at once. In your controller logic you will often have helper functions like:

```
buildPost post = post
    |> fill @''["title", "body"]
```
which will grab the `title` and `body` param keys from a request and populate the `post` record with the param values. This is all straight from the IHP docs but let us make this recycled Espresso post into a Lungo by looking at the code. The `fill` function is actually provided by the `FillParams` type class: 

```
class FillParams (params ::[Symbol]) record where
      fill :: (
           ?requestContext :: RequestContext
           , HasField "meta" record ModelSupport.MetaBag
           , SetField "meta" record ModelSupport.MetaBag
           ) => record -> record
```

The first declared instance of this type class is:

```
instance FillParams (''[]) record where
    fill !record = record
```

`FillParams` accepts two type parameters: a list of type level names `''[]` and a `record`. It also expects the `?requestContext` implicit parameter which is available in all IHP actions, and, finally, the constraints require our record has a `meta` field. In IHP the `meta` record field holds a `MetaBag` data type. This bag gives IHP a way of tracking problems with a form. A user might get most of the form "right" like provide a name, an address, etc., but they may not give a valid email address. The `MetaBag` contains the ''annotations''; basically, a place to stores notes on what went wrong (or what has been changed) so that the form can be passed back to the user with the correct fields still correct and the problematic fields highlighted. Have a look through `IHP.Controller.Param` for more details!
', 'forms, Metabag', '2020-09-29 13:56:29.021352+01', 'lungo', '1859-01-06');


ALTER TABLE public.coffees ENABLE TRIGGER ALL;


ALTER TABLE public.users DISABLE TRIGGER ALL;



ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.comments DISABLE TRIGGER ALL;



ALTER TABLE public.comments ENABLE TRIGGER ALL;


