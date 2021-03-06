

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
to the wai request, request parameters, file uploads etc.', 'Implicit Parameters', '2020-09-24 14:26:40.761952+01', 'espresso', '2020-10-06');
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

Defaults make it easy to populate a new record''s data fields in the database without worrying about writing a lot of boilerplate.', 'records, models, defaults', '2020-09-24 14:31:56.476449+01', 'irish_coffee', '2020-10-01');
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
```', 'ENUM, selectfields, Forms', '2020-09-23 21:02:55.999273+01', 'latte', '2020-09-23');
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
', 'forms, Metabag', '2020-09-29 13:56:29.021352+01', 'lungo', '2020-10-09');
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
simple to implement and secure.', 'Admin, Sessions', '2020-09-30 10:45:45.176187+01', 'freddo', '2020-10-08');
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
If the user is logged in as Admin we have purity and the IO consumes a unit and moves on. If there is no Admin logged in the client is redirected to the newSessionUrl which is typically just a login page. The `newSessionUrl` method was defined (as you''ll see in the documentation) when you made your `Admin` data type an instance of `HasNewSessionUrl`:

```haskell
instance HasNewSessionUrl Admin where
    newSessionUrl admin = "/NewSession"
```

The way the controller logic and implicit parameters are handled and connected to Views in IHP makes standard authorization/authentication patterns very concise for the programmer to write. ', 'Sessions', '2020-09-28 13:51:37.428757+01', 'americano', '2020-10-07');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('bbd05ccc-5c09-48e2-a336-a6870ce6cbea', 'Validation', 'Here is  a typical use case of `ifValid` in a Controller action creating a new record:

```haskell
    action CreateCoffeeAction = do
        let coffee = newRecord @Coffee
        coffee
            |> buildCoffee
            |> validateField #title nonEmpty
            |> validateField #body nonEmpty
            |> ifValid \case
                Left coffee -> render NewView { .. } 
                Right coffee -> do
                    coffee <- coffee |> createRecord
                    setSuccessMessage "Coffee created"
                    redirectTo CoffeesAction
```

`buildCoffee` is a helper function to fill up our record fields using the IHP `fill` function:

```haskell
buildCoffee coffee = coffee
    |> fill @["title","body","labels","coffeeType","lastDrank"]
```

Now so far we can say that `ifValid` takes a function, written as a lambda `\case...`. 

(ASIDE: It turns out `\case` is syntactic sugar for: 

```haskell
\x -> case x of 
    pattern1 -> ...
    pattern2 -> ...
```

This syntactic sugar is an extension enabled by `LambdaCase`. It is a humble extension but apparently (by usage) quite a popular one. END ASIDE)

`ifValid` takes this lambda case which branches  to either a `Left` which takes the client back to `NewView` or `Right` which updates the record and redirects them somewhere new and exciting (both branches are in the IO monad as we shall see when we look at the type signature for `ifValid`). The second argument of `ifValid` is a record (model). 
Let us include the full type signature and definition here:

```haskell
ifValid :: (HasField "meta" model ModelSupport.MetaBag) => (Either model model -> IO r) -> model -> IO r
ifValid branch model = branch ((if null annotations then Right else Left) model)
    where
        annotations :: [(Text, Text)]
        annotations = getField @"annotations" meta
        meta :: ModelSupport.MetaBag
        meta = getField @"meta" model
```

We have our branch function which will point us `Right`  if our model has `null annotations`(These are the validation notes that IHP stores on all our models in a field called `MetaBag`) or `Left` if one of the validation functions has not been passed. 

The `validateField` functions are what mark the model''s `annotations` card. `ifValid` then decides if it the model is worthy of going into the app database or the client must go back and try submitting the form again. ', '', '2020-10-09 15:36:38.710404+01', 'mocha', '2020-10-09');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('56d2fa89-349e-4da6-8501-43af331f2bf4', 'Fetch Me... Nothing.', 'Let us look at some common patterns that we might encounter in an IHP request cycle. Let us say that our database contains some site addresses stored as `Address` data. In our application we may want to let the user query an address; if the database contains the address return it; otherwise return a blank record.  Here is some `Controller action` code that would handle this (the `Address` data are assumed to have a field called `siteId` in this example):

```haskell
    action ShowAddressAction { siteId } = do
        address_query <- query @Address   
                          |> filterWhere(#siteId, siteId) 
                          |> fetchOneOrNothing       
     

        let address = case address_query of
                                       Just record -> record
                                       Nothing -> newRecord @LongAddress
                                                             |> set #siteId siteId
                                                             |> set #country "Ireland"
     render ShowAddressView {..}
```

Now, on the view side of things, our View record will look like:

```
data ShowAddressView = ShowAddressView{ address:: Address}.
```

We can think of another variation on this theme.

 Let us have a View record defined as:

```
data ShowAddressView = ShowAddressView{ address:: Maybe Address}.
```

In this variation instead of unpacking the `Maybe Address` from `fetchOneOrNothing` in the `Controller` we would have some `renderAddress:: Maybe Address -> Html` function in our `View`. This function will pattern match against two cases:

```
renderAddress Nothing = [hsx| You better head over and  <a href={UpdateAddressAction}> Update </a> the address we have nothing in the database!  |]
renderAddress Just address =  [hsx| <p> {address} </p> |]
```

The considerations boil down to whether we prefer a View that changes depending on the data it receives or a static View which receives fixed data determined by the `Controller` logic. 

IHP raises many interesting philosophical questions of this nature: just another one of its many excellent features.', 'fetch, views, fetchOneOrNothing', '2020-10-08 13:30:33.509801+01', 'flat_white', '2020-10-12');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('edb8cfe4-b59e-4a36-a3e8-dc329012e7aa', 'Second Sessions', 'Let us take another, closer, look at that authorization/login tools provided by IHP. The place to start is the IHP documentation and the documentation starts with the `IHP.LoginSupport.Helper.Controller`. 

The standard pattern for an application in IHP is to have `Users` and `Admins`. There are helper functions like `currentUser` and `currentAdmin` which themselves call `currentUserOrNothing` and `currentAdminOrNothing`. The latter functions have a return type of `Maybe User` or `Maybe Admin`. 

(At the moment there is some duplication of logic shared between functions defined on `User` and `Admin`. It may be that an abstraction like the idea of a `Role` could help refactor authentication and recycle some of the shared logic; but for present purposes a `User` and an `Admin` are sufficient.)

The key methods in `IHP.LoginSupport.Helper.Controller` are `login,logout` and , `sessionKey`. We will also mention that the `currentUser...` and `currentAdmin...` functions all have `FrameworkConfig` constraints in their type signatures. The "IHP.FrameworkConfig" module is where the default session/cookie settings are handled so if you want to see the details of what you''re putting in your cookies you can have a look and see there. 

When a user authenticates IHP generates a little note on the Session key:
```
{-# INLINE sessionKey #-}
sessionKey :: forall user. (KnownSymbol (ModelSupport.GetModelName user)) => Text
sessionKey = "login." <> ModelSupport.getModelName @user
```

`getModelName @user` will return something like "User". Then `login` sticks the `id` string on the end and the `user` has a session. This key gets erased when the user logs out. 

The bulk of the controller logic can be stuck in a `Controller/Sessions.hs` module with three methods:

```
instance Controller SessionsController where
    action NewSessionAction = Sessions.newSessionAction @User 
    action CreateSessionAction = Sessions.createSessionAction @User
    action DeleteSessionAction = Sessions.deleteSessionAction @User
```

which are all to be found in `IHP.AuthSupport.Controller.Sessions`. Of these the most involved is `createSessionAction`. It handles the following steps which we''ll just write out as a list of tasks in a haskelly kind of notation:

```
1) Query for a user record with a particular email address
2) Considers two cases:
           \case
                     User with that email exists -> Make sure that user isn''t locked out from logging 
                                                                     in too many times (failed attempts field on records tracks this),
                                                                     Verify password: 
                                                                           \case 
                                                                                   Good Password -> redirect to the logged in landing page. 
                                                                                   Bad Password -> tell them they haven''t got the right password 
                                                                                                                and increment failed attempts field. 
                     Nothing (i.e. no user/admin record with that email) -> Send them to sign up.
```

It is also useful to add an entry to the `ViewContext` record that holds`currentUserOrNothing/currentAdminOrNothing` record so that you have simple access to this data in all your `Views`. ', 'Authorization, Login, Sessions', '2020-10-12 13:08:12.126225+01', 'marochino', '2020-10-13');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('210beb5e-1191-49cf-b4e9-bd0fb0aacb99', 'Param-bulators, Call the Parambulance!', 'Let us go type hunting today. We wish to understand everything that happens when we write: 

```
let 
     username = param "username"
```

in an IHP controller.

This espresso will involve listing type signatures and very little exposition; so here goes:

```
param :: (?context :: RequestContext) => (ParamReader valueType) => ByteString -> valueType
param !name = case paramOrNothing name of
    Just value -> Either.fromRight (error (paramParserErrorMessage name)) (readParameter value)
    Nothing -> Exception.throw (ParamNotFoundException name)
```

Ok some exposition, `ParamReader` is a a type class with a `readParameter` operator that lets you specify how you want to map whatever `ByteString` comes at you over the wire to another data type e.g. `Text,Double,Int,[values]` etc. 

`param` is calling `paramOrNothing`:

```
paramOrNothing :: (?context :: RequestContext) => ParamReader a => ByteString -> Maybe a
paramOrNothing !name = case queryOrBodyParam name of
    Just value -> case readParameter value of
        Left error -> Nothing
        Right value -> Just value
    Nothing -> Nothing
```

Ok, good, so that is just unpacking `Maybe` s and `Either` s. Let us keep going `queryOrBodyParam`:

```
queryOrBodyParam :: (?context :: RequestContext) => ByteString -> Maybe ByteString
queryOrBodyParam !name = join (lookup name allParams)
```

So `queryOrBodyParams` is a function accessing `allParams` params that are parsed out of the URL and that could be passed in a request body (say from a Form):

```
allParams :: (?context :: RequestContext) => [(ByteString, Maybe ByteString)]
allParams = concat [(map (\(a, b) -> (a, Just b)) params), (Wai.queryString request)]
    where
        RequestContext { request, params } = ?context
```

IHP''s ubiquitous and helpful `?context` implicit parameter is holding some queries and `Wai.queryString request` is holding the rest. Will we keep going? Yes...

What''s Wai doing? Ok 

```
module Network.Wai.Internal where
     import qualified Network.HTTP.Types           as H

queryString          :: H.Query
```

So `queryString`  has type `H.Query` (which if we look that up is just a type synonym for `[(B.ByteString, Maybe B.ByteString)]`, which makes sense  because we have `keys` and `Maybe values` and they are all `ByteStrings` until they get IHP `readParameter` "ed". 

Right, good that''s enough for a short, sharp, punchy espresso we can look into Wai parsing strategies another day. 
', '', '2020-11-04 12:54:55.239264+00', 'espresso', '2020-11-16');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('4efdf15c-bf68-4bd2-8a9f-538a440010f8', 'Include Me', 'A SQL record may well have foreign keys on it. These relationships are in the Schema for a reason and frequently we may want to present the data from different related records in a single view. For instance let us take an example from the IHP Forum. We have a thread, which is some markdown text discussing some new fascinating IHP related feature, and we want to associate the name of the user who has contributed this pearl of wisdom, and also the name of the topic that the thread has been filed under. 

The IndexView of threads is itself a record defined like this:

```
data IndexView = IndexView { threads :: [Include'' ["userId","topicId"] Thread] }
```

Lets have a look at the `Include('')` type signature(s). 

```
type family Include (name :: GHC.Types.Symbol) model

type family Include'' (name :: [GHC.Types.Symbol]) model where
    Include'' ''[] model = model
    Include'' (x:xs) model = Include'' xs (Include x model)
```

So we have a type family `Include`, with a version denoted `Include''` that recurses `Include` along the list, applying `Include` to individual names.  
A type family is just the abstraction though. Where are the concrete instances? Well, to find these we must go to one of the true sources of IHP magic:`build/Generated/Types.hs`. There we find: 

```
type instance Include "userId" (Thread'' userId topicId comments) = Thread'' (GetModelById userId) topicId comments
type instance Include "topicId" (Thread'' userId topicId comments) = Thread'' userId (GetModelById topicId) comments
```
(GetModelByID is another helpful type family. IHP uses a few type families under the hood, `GetTableName`, `GetModelByTableName`, and`GetModelName` to fetch data Models from string names and string names from data Models have a look in `build/Generated/Types.hs` to see them in action.)

In the view we now can write something like:

```
data IndexView = IndexView { threads :: [Include'' ["userId","topicId"] Thread] }
instance View IndexView ViewContext where  
    html IndexView { .. } = [hsx|      
       {get #body thread |> renderMarkdown}
      {get #name user}
      {get #name topic}
|]
     where
          user = get #userId thread
          topic = get #topicId thread
```

What about on the Controller side? The query looks like this:

```
    action ThreadsAction = do
        threads <- query @Thread
            |> orderByDesc #createdAt
            |> fetch
            >>= collectionFetchRelated #userId
            >>= collectionFetchRelated #topicId
        render IndexView { .. }
```

The type class `class CollectionFetchRelated relatedFieldValue relatedModel` defines `collectionFetchRelated`, it takes care of the database transaction to fetch the related records. On the SQL side the following commands should be issued:

```
SELECT * FROM threads
SELECT * FROM users WHERE id IN (?)
SELECT * FROM topics WHERE id IN (?)
```

So with `collectionFetchRelated` and `Include''` we can well, fetch related records... and include them in our view! Who said naming things was hard?
', 'Include, collectionFetchRelated ', '2020-10-12 19:37:06.96898+01', 'mocha', '2020-10-14');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('57451821-29e3-44d2-b103-205a74acefca', 'Lights Camera, action', 'Lets have a look at an `action` in IHP. This is a method introduced by the `Controller` type class:

```
class (Show controller, Eq controller) => Controller controller where
    beforeAction :: (?controllerContext :: ControllerContext, ?modelContext :: ModelContext, ?requestContext :: RequestContext, ?theAction :: controller) => IO ()
    beforeAction = pure ()
    action :: (?controllerContext :: ControllerContext, ?modelContext :: ModelContext, ?requestContext :: RequestContext, ?theAction :: controller) => controller -> IO ()
```

So the lowercase controller has a couple constraints on it; namely `Show` and `Eq`.  (This explains why in `Web/Types.hs` we define the data constructors so that they are all `... deriving (Eq, Show, Data)`). Now, `action` consumes a controller and returns IO(). That seems natural enough.
It acts as a place holder so that when the App gets a request IHP can pattern match the request URL, params etc. and run the appropriate action, then return to IO(), rinsed and ready to repeat. 

The action is where the action happens so you can hang anything you want on it: a simple do that renders a view or complicated sequence of database queries and analysis. 

', 'action, Controller', '2020-10-15 13:53:37.659863+01', 'espresso', '2020-10-16');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('2c4d7424-8e70-42a5-adcb-2f45be134f64', 'Behind The IHP Fridge', 'For today''s coffee we will suggest a few stops on a self-guided tour of the IHP Developer tools. These tools are the part of the IHP package, essentially an IHP application itself, which allow you to develop your own IHP applications.

The first module (stop) on the tour is `IHP.IDE.ToolServer.Types`. This may look very familiar. It has essentially the same layout as you would expect in your Application''s `Web/Types.hs` module. Instead of your application/business data structures this modules defines *the abstract data structures of your application/business data structures* ; `SchemaController`, `TablesController`, `ColumnController` etc. We''re going up to the source of the Nile here; deeper into the jungle of abstraction. These IHP `ToolServer` data types have been designed to make it easy to generate your more application specific data types. You might wonder that if we journey far enough up the river whether we will find some abstract source of all abstract data structures; a generator of IHP Application generators perhaps? We should be careful with such speculations. We don''t want to put web developers out of work.

The next stop on the tour is `IHP.IDE.LiveReloadNotificationServer`. The reality of the world situation is that we can''t actually do everything in Haskell. In a certain sense real purity would suggest, to this author at least, that we shouldn''t be able to do anything with Haskell. Fortunately the IO Monads and philosophical compromises have been made so that Haskell can interact with the outside world. The `LiveReloadNotificationServer` module organizes some of the communication logic between the pure and safe world of a Haskell development server and the grim real world of your browser and your impure application development intentions. A second cup of Coffee is probably warranted to describe how the javascript and the websockets interact and the details of that interaction so we shall return at a later date. You need only glance at this module before we move on to the final stop on this tour.

Let us have a quick look at the`ihp/exe/IHP/IDE/DevServer.hs` script. This runs along in the background while you are developing your application. It is a `forever` (see the definition of `forever` in Control.Monad) script. That means the script potters along accepting messages from the App running in your Browser as a client, it propagates the DevApp State, sends messages to the browser telling it to refresh on any changes, and if you are in Debug Mode (entered by running the `./start` script with an environment variable e.g. `DEBUG=1 ./start`) it prints status info to the terminal to give you an idea what it is doing. It does this forever or until you unplug your computer or hit Ctrl-C. 

The DevServer design is quite concise considering all that it enables. ', 'Devserver', '2020-10-14 12:11:07.262344+01', 'lungo', '2020-10-15');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('11367669-5542-49cb-8d72-23c91e6e1a3d', 'Let''s Put This In Context', 'It''s another Mocha today; we''re talking Hot Chocolate, Cream, Marshmallows, Coffee, the works. Maybe someday we''ll get back to serving short sharp espressos but in these autumn months (that is, the months when this is being written not necessarily read) we have time to sip our drinks leisurely. 

We''re going to look at a part of IHP that is involved in both Authentication and LiveReloading. We''re going to put things in context. And to put things in context we need a context. I mean we need to initialize a context so enter stage right `initContext`:

```haskell
instance InitControllerContext WebApplication where
    initContext =
        initAuthentication @User
```

Hoh, wow we''re in the `FrontController`,  OK, it''s not *quite the* `./Main.hs`, but it''s getting pretty high up in our IHP directory structure. 

There are a few things going on here. Let''s think of `WebApplication`  as our application''s  controllers and views stored in the `Web` directory. This could also be an `Admin` directory that contains the controllers and views in the administrative part of your project. (For those who have worked with Flask before this layout is similar to their notions of Blueprints). Your project may have `WebApplication, AdminApplication, etc.` and this is the way the Model/View/Controller patterns get organized in IHP for naming and routing etc.

Now the `initAuthentication` function comes from `IHP.LoginSupport.Middleware`:

```
initAuthentication context = do
    user <- getSessionUUID (sessionKey @user)
            >>= pure . fmap (Newtype.pack @(Id user))
            >>= fetchOneOrNothing
    pure (TypeMap.insert @(Maybe (NormalizeModel user)) user context)
```

So this pulls a User `UUID` from the current session as some text, binds it nicely into the `Id user` type (remember a user here doesn''t have to be `User` it could be `Admin` or `RookieUser` or whatever record type is defining the person logging in to the application), and binds all that to a `fetchOneOrNothing` from the database. Then it stores the (`Maybe Type`) in a context map. 

So now we know how the `user` type got registered in the `ControllerContext` and why we can run an IHP function like `currentUserOrNothing` which itself calls `maybeFromControllerContext`. It''s all the story of how the `ControllerContext` got its associated map of types. 

Before finishing let us just check what `initContext` was doing before we started messing with it:
```
class InitControllerContext application where
    initContext :: (?modelContext :: ModelContext, ?requestContext :: RequestContext, ?applicationContext :: ApplicationContext) => TypeMap.TMap -> IO TypeMap.TMap
    initContext context = pure context
```

Well, holy... sh-, this seems like one of those many Haskell zen moments where it''s Morpheus from the matrix saying  "What if I told you `initContext context` was just `pure context`. 

Anyway we can wrap this up with some `AutoRefresh` musings. Part of what needs to be done to get `AutoRefresh` operational is adding some more functionality to the initContext call:

```
instance InitControllerContext WebApplication where
    initContext =
        initAuthentication @User
        >=> initAutoRefresh -- <----- ADD THIS LINE
```


Clearly `initContext` has to do a few things and chaining monads together is an efficient and extensible way of composing these tasks. Another IHP functionality that can be stacked into the context is ` >=> Modal.initModal`. No doubt there can be others.

The stacking of the chain of `initPattern` tasks in `initContext` ( the usual `initPattern` is wrapping up a `TypeMap` in `IO`, `(TMap -> IO Tmap)`) is accomplished using the the infix operator `>=>`. This operator has the handy short form name: "Left-To-Right Composition of Kleisli Arrows" and its type signature is:

```
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
```

Which, to remind us, if you hoogle, you''ll see an example of the Kleisli with `do` notation:

```haskell

do b <- bs a
   cs b
```

Fin.', 'initControllerContext', '2020-10-16 13:40:44.778269+01', 'mocha', '2020-10-19');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('e25edc82-2e84-429f-b201-21e6a4b5d199', 'IHP''s Nix Ecosystem', 'IHP as it is (currently) called stands for `Integrated Haskell Platform`. It is a Platform because you can design rocket ship applications that blast off from it, and it is integrated because, I''m just guessing here, but, because its tools are intimately integrated with the Haskell ecosystem.

IHP manages all the various environmental and module dependencies that can arise when you are developing an application that requires postgres, ghc, and an arbitrary configuration of new, old, and non-standard packages, by using `nix`. We can take as `nix` as referring to a language, an operating system, a build tool and package manager depending on the context and what is its most appropriate name given the context. 

So what is the set up? Well, at the top of your project''s directory structure you have a file called `default.nix`. If you have a look in this file you will see it is written as a standard `nix` formula:

```
let 
    ihp = builtins.fetchGit {url=..., rev=...};
    haskellEnv = import "${ihp/NixSupport/default.nix}" {
                              ihp = ihp;
                              haskellDeps = p: with p; [ ... ];
                              otherDeps =p: with p; [...];
                      projectPath = ./.;
                     };
in 
        haskellEnv
``` 

For those of you, like me, who have very little experience with nix might enjoy a little exposition here. `builtins` is  a set of functions and values that you can bring in to your nix formulae. Here we are using `builtins.fetchGit args` where the arguments can be `url,name,rev,ref` referring respectively to the URL of the repo, the name of the dir the repo should be exported to in the store (default URL basename), the git revisions, the git ref (usually a branch or tag name with HEAD as the default). This takes care of fetching our latest version of IHP from github, binding it to `ihp` and then importing the "master plan" default.nix formula from IHP. 

Have a look at that file in "ihp/NixSupport/default.nix". At the top of the file is a set of arguments (I''m just thinking of `.nix` files themselves as lambdas that we can compose):

```
{ compiler ? "ghc883", ihp, haskellDeps ? (p: []), otherDeps ? (p: []), projectPath ? ./. }:
```

The `ihp/NixSupport/default.nix`

We''ll summarize by highlighting the three nix formulas that will be consumed to build your IHP application:

```
./projectPath/default.nix -> ihp/NixSupport/default.nix -> ./projectPath/Config/nix/nixpkgs-config.nix`
```

The call to `stdenv.mkDerivation{ }` comes from `ihp/NixSupport/default.nix`. It''s kind of the main event and it specifies the build rules. 

(We''ll also mention that the final `.nix` formula also pulls in the special formulae you may have defined in `./Config/nix/haskell-packages/`. These usually come from wanting an older version of a package then the one provided for by the IHP package set. They  are generated using `cabal2nix` see the IHP docs `Advanced: Using a Different Version of a Haskell Package`.)

Looking through them in more detail will give you an idea of how IHP approaches package management and brings everything together so that you can run your application by typing `./start`.
', 'nix', '2020-10-19 13:30:08.308049+01', 'americano', '2020-10-30');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('c2392fec-1159-4037-9bf8-0ad16917d76e', 'Ready, set, get, Vinyl Record!', 'Defining a record data type in Haskell is handy because we get a set of functions that can access the fields of the record. One problem though is if we have two different records with the same field name those function definitions are going to clash. 

For instance:

```
Flower {name::String, Colour :: String}
TeenageMutantNinjaTurtle {name::String, weapon::String}
```

Is the name function going to accept the type of a `Flower` or a `TeenageMutantNinjaTurtle`? Do we have a `Rose` or `Donatello`? The situation is confusing. Fortunately, there has been progress, and progress continues, with routing around this issue. One way of handling the name class is by using the `OverloadedRecords` language extension.

In IHP the idiomatic way of accessing field names on models is to use `get` (which calls `Record.getField` under the hood):

```
get :: forall model name value. (KnownSymbol name, HasField name model value) => Proxy name -> model -> value
get _ record = Record.getField @name record
```

Here is an example:

```
let turtle = TeenageMutantNinjaTurtle {name="Donatello", weapon="Nunchaku"}
get #name turtle
```
But what is the problem with that code? Take a moment to see if you can spot the issue...

That''s right! Donatello used a Bo staff not Nunchaku! Let us update the record to fix that:

```
turtle
       |> set #weapon "Bo Sai" 
```

Here is what set looks like:

```
set :: forall model name value. (KnownSymbol name, SetField name model value) => Proxy name -> value -> model -> model
set name value record = setField @name value record
```

The third member of this family is modify which allows you to map a function onto a record value to transform it into a new value:

```
modify :: forall model name value updateFunction. (KnownSymbol name, Record.HasField name model value, SetField name model value) => Proxy name -> (value -> value) -> model -> model
modify _ updateFunction model = let value = Record.getField @name model in setField @name (updateFunction value) model
```

In all these methods the interesting constraint is:

```
class HasField x r a | x r -> a where
     getField :: r -> a
```

from `GHC.Records`. This constraint ties in with the `OverloadedRecordFields` language extension.
Records and dot syntax seem to be a bit of a philosophical rabbit hole for Haskellers so a little googling will take you into some pretty heated discussion forums. Fortunately in IHP all you need to worry about is how to `get`, `set` and `modify`.   ', 'Record, get, set, modify', '2020-10-30 12:46:24.486412+00', 'americano', '1858-11-17');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('eb000f34-d9c4-410c-8629-a5b0c118b33b', 'IHP has it: Under Control', 'Can you remember when IHP had a module like `Web/View/Context.hs`? And it defined ViewContext as an instance of ViewSupport.CreateViewContext? And it defined some type synonyms and bound `FlashMessages` to your `ViewContext` and you could also extend that ViewContext record to include more data like the current User etc.? No? 

Well. Good. Because it''s gone now. 

The whole `ViewContext/ControllerContext` structure has been rationalized into a single implicit parameter `?context` with type `ControllerContext` (defined in `IHP.ControllerSupport`) to the great benefit of reduced syntactic clutter across the whole IHP framework. It is a development to be celebrated. 

This is the data type:

```
data ControllerContext = ControllerContext { requestContext :: RequestContext, customFieldsRef :: IORef TypeMap.TMap }
                       | FrozenControllerContext { requestContext :: RequestContext, customFields :: TypeMap.TMap }
```

(A Historical Note: These structures were introduced when the `FrameworkConfig` type class was refactored into a record data type).

Let''s see how it might work in practice. 

Here is an example (straight from the top brass at IHP HQ;) of the sort of thing you may like to do with your implicit `?context` parameter. 

Take language settings as an example. Say you were interested in spinning out a cool IHP app for your english and... perhaps, Norwegian speaking customers. You could accomplish this by defining a Language data type, 

```
data Language = No | En

setLanguage :: (?context :: ControllerContext) => Language -> IO ()
setLanguage language = putContext language
```

Now in your `Controller`:

```
action MyAction = do
    setLanguage No
```

And now your view can access these data fields with `fromFrozenContext` safely:

```
currentLanguage :: (?context :: ControllerContext) => Language
currentLanguage = fromFrozenContext @Language
```', '', '2020-11-16 14:42:39.623771+00', 'americano', '2020-11-17');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('515f482a-f607-4252-b2c1-60a843a7cb89', 'Papa''s Got a Brand New MetaBag', 'Lets say you wish to copy an IHP record into a new record and insert that into a table. Maybe the use case is you are cacheing some record and when a new user comes along you want to reuse most of the data in that record with a couple superficial changes without going through the trouble of regenerating all of that record''s fields. Your first attempt might look like:

```
existingRecord < query @Model
                  |> filterWhere (#someField, someValue)
                  |> fetchOneOrNothing
... 
case existingRecord of
      Nothing -> createRecordNormally
      (Just someSite) -> do
                          someSite <- someSite 
                                           |> set #someField "smallChange" 
                                           |> createRecord
```

That should just update the "someField" Column and leave all the other columns filled with the useful data that was already stored in the record right? Right? 

Of course not. You have forgotten the `MetaBag`. 

In `ihp/IHP/ModelSupport.hs` the `MetaBag` is used for tracking changes to a record. This is very useful when we want to do tricks with Form Validation. Here is the MetaBag data structure:

```
data MetaBag = MetaBag
  { annotations :: [(Text, Text)]
  , touchedFields :: [Text]
  } deriving (Eq, Show)
```

Some helper functions access the `MetaBag` so we can check when fields are modified:

```haskell
didChangeRecord :: (HasField "meta" record MetaBag) => record -> Bool
didChangeRecord record =
    record
    |> get #meta
    |> get #touchedFields
    |> isEmpty
```


As we saw one consequence of this comes when copying a record. If the fields of our newRecord are untouched (as recorded by the MetaBag) the default values will populate them. 

The solution is to set every field of the copied record to the value of the original record. This can be accomplished using new record and a lot of boilerplate:

```
copiedRecord <- newRecord @Site
    |> set #fieldA (get #fieldA originalRecord)
    |> set #fieldB (get #fieldB originalRecord)
    ...
```

In time there might be a SYB solution like this:

```
let copiedRecord = newRecord @Site
                                           |> copyFields @["fieldA", "fieldB"] otherSite
```', '', '2021-01-08 14:10:25.829369+00', 'flat_white', '2021-02-09');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('d45de61c-bb2e-47c5-b8e8-ce5537a0276b', 'Ice Cold Authenticity', 'Of immense use to any `IHP`er is the family of  `currentUserOrNothing`, `currentAdminOrNothing` functions. With IHP''s rationalized `?context` parameter we would like to see how the sessions get initialized and the currently logged in user types gets passed around with the `?context`.  The logic is collected together in `IHP.LoginSupport...`.

The top level function that takes care of authentication is `initAuthentication`. 

Which you pack in to your FrontController Logic:

```
initContext = setLayout defaultLayout >=> initAuthentication @User
```

The `initAuthentication` function is to be found in `IHP.LoginSupport.Middleware`:

```haskell
initAuthentication :: forall user.
        ( ?context :: ControllerContext
        , ?modelContext :: ModelContext
    ) => IO ()
initAuthentication = do
    user <- getSessionUUID (sessionKey @user)
            >>= pure . fmap (Newtype.pack @(Id user))
            >>= fetchOneOrNothing
    putContext user
```

(I''ve dropped a number of the additional constraints on this `initAuthentication` signature as they relate to constraints on the records fields). When your session is initialized IHP makes a little note on the client''s cookie jar. `getSessionUUID` fetches the key from the cookie jar packs it into the cooke jar, fetches the user record from the database and pushes the user into the `(?context)` parameter (which is basically a `[key,value]` map of `[Types, Values]`). 

Here''s what `putContext` looks like:

```haskell
putContext :: forall value. (?context :: ControllerContext, Typeable value) => value -> IO ()
putContext value = do
    let ControllerContext { customFieldsRef } = ?context
    modifyIORef customFieldsRef (TypeMap.insert value)
    pure ()
```

Then in your Controller (or View if you "freeze" the context with some modifications) you can access the current `user` with:

```
currentUserOrNothing :: forall user. (?context :: ControllerContext, HasNewSessionUrl user, Typeable user, user ~ CurrentUserRecord) => (Maybe user)
currentUserOrNothing = case unsafePerformIO (maybeFromContext @(Maybe user)) of
    Just user -> user
    Nothing -> error "currentUserOrNothing: initAuthentication @User has not been called in initContext inside FrontController of this application"
```


where the "freezing" comes in:

```haskell
maybeFromContext :: forall value. (?context :: ControllerContext, Typeable value) => IO (Maybe value)
maybeFromContext = do
    frozen <- freeze ?context
    let ?context = frozen
    pure (maybeFromFrozenContext @value)
```

', 'Authentication, Authorization', '2020-11-17 11:48:42.919111+00', 'americano', '2020-11-25');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('27bf79af-baf3-4029-a4b2-63d9262d3c6f', 'Java, JavaScript, XCafe, CoffeeScript; Is the Computer/Caffeine Motif Getting Tired? No, We Just Need More Coffee!', 'Standard IHP integrates a few javascript packages: JQuery, Morphdom, and TurboLinks amongst them. We''ll skip the intro to JQuery and mention what TurboLinks and Morphdom are about. Some document fetching and parsing yields the following:

Turbolinks Docs
```
Turbolinks® makes navigating your web application faster. Get the performance benefits of a single-page application without the added complexity of a client-side JavaScript framework. Use HTML to render your views on the server side and link to pages as usual. When you follow a link, Turbolinks automatically fetches the page, swaps in its <body>, and merges its <head>, all without incurring the cost of a full page load.
```

Morphdom Docs
```
This module was created to solve the problem of updating the DOM in response to a UI component or page being rerendered. One way to update the DOM is to simply toss away the existing DOM tree and replace it with a new DOM tree (e.g., myContainer.innerHTML = newHTML). While replacing an existing DOM tree with an entirely new DOM tree will actually be very fast, it comes with a cost. The cost is that all of the internal state associated with the existing DOM nodes (scroll positions, input caret positions, CSS transition states, etc.) will be lost. Instead of replacing the existing DOM tree with a new DOM tree we want to transform the existing DOM tree to match the new DOM tree while minimizing the number of changes to the existing DOM tree. This is exactly what the morphdom module does! Give it an existing DOM node tree and a target DOM node tree and it will efficiently transform the existing DOM node tree to exactly match the target DOM node tree with the minimum amount of changes.
```

So that gives us some idea what those modules are up to. Now in `lib/IHP/static/helpers.js` you''ll find a bit of the behind the scenes magic. For instance this block of javascript:

```
document.addEventListener(''DOMContentLoaded'', function () {
    initDelete();
    initDisableButtonsOnSubmit();
    initBack();
    initToggle();
    initTime();
    initDatePicker();
    initFileUploadPreview();
});
```

So, for instance, when you see a link with the `js-delete` class in IHP, its the `initDelete()` javascript function in the `helpers.js` which catches the click on the link, turns it into a form with a `POST` request and does all that `XMLHttpRequest()` stuff that might get annoying to do yourself after a while.

Now this configuration works very (very) well if we are purely rendering html markup. Scroll bars don''t go jumping around text doesn''t swap in and out only small changes in the page state get updates and it feels seamless. Troubles may start though when we leave the purity of haskell rendering html and start grafting our own `<script>...</script>` tags throughout the innerHtml. Handling this may require a little more consideration.', 'Javascript', '2020-11-25 10:25:10.09803+00', 'americano', '2020-12-16');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('e7a8a434-beeb-498e-aa8a-a64e966e3fef', 'The IHP Environment', 'Programming in Haskell is always enjoyable. After an afternoon of coding the programmer should feel relaxed. In much the same way they might be if they had just been to a Russian sauna sipping on vodka and snacking on salted fish. The developer experience experience can be enhanced with the constantly improving haskell-language-server and ghcide (ghc- integrated development environment). New users to the nix/IHP framework who have suffered before building ghc and the haskell-language server from scratch and by hand may be pleasantly surprised by the way language support works out of the box. 

The magic is to be found in the `build/ihp-lib/.hie-bios` file of your IHP project.

```
# Used by haskell-language-server to find GHC settings
cradle:
    bios:
        program: build/ihp-lib/.hie-bios
```

From the `hie-bios` docs we find:

```
hie-bios is the way to specify how haskell-language-server and ghcide set up a GHC API session.
```

(bios stands for basic-input-output-system).

With `<nixpkgs>` and the `digitally-induced cachix` you can quickly build a pinpointed version of `ghc`, and `IHP`, and any additional Haskell packages you want. You can then have them all collected in your nix store and you can load your projects environment with a single call to `nix-shell`. 

The IHP `Makefile` provides a build rule `make print-ghc-options`, which outputs all the required language extension flags, and `module include paths` you can populate the `.hie-bios` with all the flags and relative `libdirs` required to satisfy the `haskell-language-server`. With a satisfied language server the task is then to pick your favourite editor (which simply requires some reflection and soul searching on the part of the user). 

Once the text editor has been chosen it can be configured it to link with the haskell language server and derive the benefits of editor prompted code completion, type signatures, hlints, import warnings, and syntax highlighting.', '', '2020-12-16 15:07:45.08429+00', 'espresso', '2021-01-08');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('0efd3f73-45bd-401e-95f0-a49f8dc54ccf', 'The Frames', 'Let''s have a quick look at the `FrameworkConfig` data structure. 

```
data FrameworkConfig = FrameworkConfig 
    { appHostname :: Text
    , environment :: Environment
    , appPort :: Int
    , baseUrl :: Text
    -- | Provides IHP with a middleware to log requests and responses.
    -- By default this uses the RequestLogger middleware from wai-extra. Take a look at the wai-extra
    -- documentation when you want to customize the request logging.
    -- See https://hackage.haskell.org/package/wai-extra-3.0.29.2/docs/Network-Wai-Middleware-RequestLogger.html
    -- Set @requestLoggerMiddleware = \application -> application@ to disable request logging.
    , requestLoggerMiddleware :: Middleware
    -- | Provides the default settings for the session cookie.
    -- - Max Age: 30 days
    -- - Same Site Policy: Lax
    -- - HttpOnly (no access through JS)
    -- - secure, when baseUrl is a https url
    -- Override this to set e.g. a custom max age or change the default same site policy.
    -- > sessionCookie = defaultIHPSessionCookie { Cookie.setCookieMaxAge = Just (fromIntegral (60 * 60 * 24 * 90)) }
    , sessionCookie :: Cookie.SetCookie
    , mailServer :: MailServer
    , databaseUrl :: ByteString 
    -- | How long db connection are kept alive inside the connecton pool when they''re idle
    , dbPoolIdleTime :: NominalDiffTime
    -- | Max number of db connections the connection pool can open to the database
    , dbPoolMaxConnections :: Int
    -- Override this if you use a CSS framework that is not bootstrap
    , cssFramework :: CSSFramework
}
```

We can add options to the ConfigEnvironment which can be thought of as a map of (Key,Value) pairs using
the `option` function:

```
option :: forall option. Typeable option => option -> State.StateT TMap.TMap IO ()
option value = State.modify (\map -> if TMap.member @option map then map else TMap.insert value map)
```

So, for example, if you have a look at `Config/Config.hs` you will see:

```
config :: ConfigBuilder
config = do
        option Development
        option (AppHostname "localhost")
```

(This makes use of the type synonym: `type ConfigBuilder = State.StateT TMap.TMap IO ()`).
Where the Development and Apphostname data types are being dropped into our Config environment.

The inverse of `option` is `findOption`:

```
findOption :: forall option. Typeable option => State.StateT TMap.TMap IO option
findOption = do
    options <- State.get
    options
        |> TMap.lookup @option
        |> fromMaybe (error $ "Could not find " <> show (Typeable.typeOf (undefined :: option)))
        |> pure
```

These structures and functions are worth a look at because the whole IHP party starts of when you `run` the server with the lines:

```
run :: (FrontController RootApplication, Job.Worker RootApplication) => ConfigBuilder -> IO ()
run configBuilder = do
    frameworkConfig <- buildFrameworkConfig configBuilder
```

So having some idea of what is going on here will get us off on the right foot. 
', '', '2020-11-17 10:31:35.841355+00', 'espresso', '2020-11-20');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('bcadd2fe-64ea-45fb-8137-60ab9a10b926', 'What are my Options?', 'The `./Config` module is where all configuration settings can be conveniently set. Often you''ll want to hang API/authentication keys here, and set what mode your application is in etc. 
 
```
module Config where

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")
    option $ SES
        {
          accessKey = "BEZJOSFFE"
        , secretKey = "cHUMEgoDnioC!"
        , region = "eu-west-1" 
        }
```

In a view these options are made accessible by freezing a "Type dictionary".  Each `option DataConstructor` call is adding a new key:value to the implicit `?context` variable. The `?context` variable is passed to all your views under the hood (implicitly). 

Besides this extensible dictionary of option Types there is also the core `FrameworkConfig` record. This data structure contains standard structures that all applications will need e.g. the `baseUrl`, `databaseUrl`, `cssFramework`. There is a nice utility function : 

```
fromConfig :: (?context :: context, ConfigProvider context) => (FrameworkConfig -> a) -> a
    fromConfig selector = (selector . getFrameworkConfig) ?context
``` 

where `selector` accesses one of the fields of the `FrameworkConfig` record. So if you need to grab your app''s production URL (say you are sending an automated email with a link to the site) at any point you can access it as:

```
[hsx| The site is hosted <a href={fromConfig baseUrl}> here </a>. |]
```
', '', '2021-02-10 10:50:41.054875+00', 'espresso', '1800-01-01');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('9b008877-eb6f-442f-b08a-cb637e9e8c29', 'Du Hast IHP-Mail', 'Let''s say that you are a young and aspiring merchant of books named Megan and you need to compete with a new start up book merchant named Tom. Tom has a "Website" on the "Internet" where "Users" can create an account and then order books. You have a plan to one up his application. When "People" (you (quite rightly) prefer not to refer to your customers as "users") sign up to your website you are going to automatically send that Person an email. Hopefully this feature will run Tom out of business and leave you as the sole proprietor of books in the region. 

IHP can help. 

The following pattern follows a few of the usual steps for incorporating common application features with IHP. We''ll look at the configuration and the data types required to start automatically sending emails. 

We''ll do some configuration first in `Config.hs`:

```
import IHP.Mail

config :: ConfigBuilder
config = do
    -- other options here, then add:
    option $ SMTP
        { host = "smtp.myisp.com"
        , port = 2525
        , credentials = Just ("megan", "nottheonefromthatfilm")
        }
```

SMTP stands for Simple Mail Transfer Protocol.  In the `Config.hs` `SMTP` is a constructor of the
`MailServer` data type defined in `IHP.Mail.Types`: 

```
data MailServer =
    -- | Uses AWS SES for sending emails
    SES { accessKey :: ByteString
        , secretKey :: ByteString
        --  | E.g. @"us-east-1"@ or @"eu-west-1"@
        , region :: Text }
    -- | Uses the local Sendmail binary for sending emails. Avoid this with IHP Cloud
    | Sendmail
    -- | Uses SendGrid for sending emails
    | SendGrid { apiKey :: Text
               , category :: Maybe Text }
    -- | Uses a generic SMTP for sending emails
    | SMTP { host :: String
           , port :: PortNumber
           -- (Username,Password) combination
           , credentials :: Maybe (String, String)}
```

As you can see there are a number of email configurations you can choose from depending on 
the mailserver you want to use. You (Megan) might consider putting in a pull request for an AOL configuration.

Internally, IHP uses many of the utility functions for sending mail
are imported from the  `Network.Mail.SMTP` package on hackage.

Now you (still Megan) want to customize your emails based on some of the details 
that the person signing up has given so you define a record type: 

```
data ConfirmationMail = ConfirmationMail { person :: Person, offer :: Offer}
```

Then you make that record type an instance of the `BuildMail` type class from `IHP.Mail`:

```
class BuildMail mail where
    -- | You can use @?mail@ to make this dynamic based on the given entity
    subject :: (?mail :: mail) => Text
    
    -- | The email receiver
    --
    -- __Example:__
    -- > to ConfirmationMail { .. } = Address { addressName = Just (get #name user), addressEmail = get #email user }
    to :: mail -> Address

    -- | Your sender address
    from :: (?mail :: mail) => Address

    -- | Similiar to a normal html view, HSX can be used here
    html :: (?context :: context, ConfigProvider context) => mail -> Html

    -- | When no plain text version of the email is specified it falls back to using the html version but striping out all the html tags
    text :: (?context :: context, ConfigProvider context) => mail -> Text
    text mail = stripTags (cs $ Blaze.renderHtml (html mail))
```

This type class brings in to scope all the utility functions required for setting the subject, address, and content of the email:
 
```
instance BuildMail ConfirmationMail where
    subject = "Subject"
    to ConfirmationMail { .. } = Address { addressName = Just "Tom", addressEmail = "tom@booksale.ie" }
    from = "megthebookseller@aol.com"
    html ConfirmationMail { .. } = [hsx|
        <span class="greetings"> Dear {get #name person} <span>,
        <div> You may be interested in the following books: {renderOffer offer}. </div> ...
    |]
```

Finally you update your Controllers to fire the the email off:

```
action NewPersonAction { personId } = do
    person <- fetch personId
    offer <- fetchOne @Offer
    sendMail ConfirmationMail { person, offer }
```

Which fetches the person and that month''s offers list and sends the email. 

Good luck Megan!', 'mail', '2021-01-28 16:08:19.974935+00', 'mocha', '2021-02-10');
INSERT INTO public.coffees (id, title, body, labels, created_at, coffee_type, last_drank) VALUES ('7859679b-0f24-4c70-8b81-2041503287e6', 'My Nixxy Valentine', 'Today''s iced coffee is all about IHP and nix. And like all stories it is a love story. We have discussed nix before in general terms but as IHP has rounded off its edges and streamlined its deployment it is worth taking another look at the way the  build is expressed. 

We begin in the ihp-boilerplate application layout. Let''s assume in what follows we have run an `ihp-new ihp-nix` to generate a new IHP application. We will follow the expressions required to build the project. 

```

```

p.s. If you notice a delay when you run `nix-shell` each time you are launching your application it is because nix is re-evaluating the whole expression each time you load your environment. Its supposed to do that! As nix flakes become the norm users will be able to cache the build results. ', '', '2021-02-12 16:33:23.884009+00', 'iced_coffee', '1858-11-17');


ALTER TABLE public.coffees ENABLE TRIGGER ALL;


ALTER TABLE public.users DISABLE TRIGGER ALL;



ALTER TABLE public.users ENABLE TRIGGER ALL;


ALTER TABLE public.comments DISABLE TRIGGER ALL;



ALTER TABLE public.comments ENABLE TRIGGER ALL;


ALTER TABLE public.nix_coffees DISABLE TRIGGER ALL;

INSERT INTO public.nix_coffees (id, title, body, labels, created_at, last_drank, coffee_type) VALUES ('f4e25484-2ec2-441f-86a0-02bbd12c8cda', '<nixpkgs>', 'No matter where you go on your first visit to `nix` whenever looking through a nix
expression you''ll likely see references to `<pkgs>` or `<mypkgs>`.

```
    let
        pkgs = import <nixpkgs> {};
```

```
    pkgs ? import <nixpkgs> #import the nixpkgs set.`
```

The Nix Packages collection (`<nixpkgs>`) is a set of derivations (+40,000 and counting)
for all sorts of packages with support for linux and MacOS.
You can see what is in them here: [Search Packages](https://search.nixos.org/packages).
You could also go to the [github repo](https://github.com/NixOS/nixpkgs) itself and check it out or browse through it online.
It is a true market bazaar of packages. Nix expressions are contributed from the four corners of the globe.
Whatever software package you wish to build you will likely find
a nix expression that will help you build that package [`<nixpkgs>`](https://nixos.org/#asciinema-demo-cover),
and if one doesn''t exist you can write your own.', '', '2021-02-15 13:41:10.306889+00', '1858-11-17', 'americano');
INSERT INTO public.nix_coffees (id, title, body, labels, created_at, last_drank, coffee_type) VALUES ('5ba7e224-c13c-4a9c-9f65-8df17be658da', 'One Afternoon...', 'Let''s say that one long and drowsy afternoon you have been trying to compile
a large program using `gcc` for hours. The compilation fails over and over. You have searched on the internet for people who
have received similarly cryptic error messages. You have waded through message boards.
Strange linking errors continue to abound, you think you have compiled successfully
and then you get even stranger run time errors. You wonder for the ninth time
whether you have been using different versions
of a C compiler, or passed the wrong compiler flags, or have forgotten to declare
an environment variable.

You type `ls /usr/local/bin` to see what you are compiling with
and you inspect the contents of that directory:

```
    ...
    gcc4
    gcc4.1
    gcc4.1.1
    ...
    _gcc4.2.3
    ...
    gcc4.3.2
```

you stare at that for a moment, a thought forms on the edge of your mind,
"I have but one life.", and then something
that had been crystallizing for years inside you takes its final shape,
you strike the desk with the palm of
our hand and cry "No more, I will abroad!".

You ring the travel agent and before you are halfway down the street
you have booked the flight to `nix`.', '', '2021-02-15 13:05:03.909733+00', '2021-02-15', 'americano');
INSERT INTO public.nix_coffees (id, title, body, labels, created_at, last_drank, coffee_type) VALUES ('3f4c7fd2-99dd-4077-ae70-487eda87f147', 'Nix Preliminaries', 'There are a few things you should know about `nix`.

Firstly, the language. They don''t speak C or any of the
other imperative languages.
The native tongue is more in the family of the
romance(functional) languages
like Spanish(Haskell) or Italian(OCAML), but distinct.

Actually, it''s a bit like Romanian, some things are familiar,
and then some inflections and constructions are not, and the overall
impression is of something vaguely comprehensible
but with a sense of erudition and lazy pragmatism.

You''ll notice `nix` is a busy country. It is also a young country.
with an active and perpetual [discourse](https://discourse.nixos.org).

Things are quite exciting in nix. People can wander in and out
of building sites as they please.

The country is dynamic and it is constantly evolving.
There are always decisions to be made collectively; but
at its core it is built on notions from functional programming; with an emphasis on purity
and reproducibility. Take a look at [Eelco Doelstra''s thesis](https://edolstra.github.io/pubs/phd-thesis.pdf)
to learn more and this [publication](https://dl.acm.org/doi/abs/10.1145/1411203.1411255).

If you are interested in composing ideas with expressions built on the
[granite bedrock of functional programming](https://www.microsoft.com/en-us/research/podcast/functional-programming-languages-pursuit-laziness-dr-simon-peyton-jones/)
and participating in what is really a transformation in the way software is built
and deployed you might feel at home with `nix`.', '', '2021-02-15 13:37:14.146859+00', '1858-11-17', 'americano');
INSERT INTO public.nix_coffees (id, title, body, labels, created_at, last_drank, coffee_type) VALUES ('81ca19ce-57ba-450f-8252-4f853977e9f6', 'Watching TV in nixland: nix channels', '`nixpkgs` is essentially a monolithic
git repo containing a directory tree of nix expressions.
The [unstable channel](https://hydra.nixos.org/job/nixpkgs/trunk/unstable)
contains the latest set of packages. The citizens of nix are busy everyday
modifying and extending the expressions in the `nixpkgs` so that you can
check out and compile everything you want using nix
from a patched version of `cat` to `libcairo`, or the entirety of `ghc`.


We can observe the [status](https://status.nixos.org/) of the mainstream `nixpkgs` channels
and some more [details](https://hydra.nixos.org/job/nixpkgs/trunk/unstable#tabs-constituents)
about the status of the builds.

When you are subscribed to the channel updates begin when changes are pushed to
master/release XX.XX branches.

```
    nix-channel --update
```

You can add new channels via:

```
    nix-channel --add https://some.channel/url my-alias
```

The `Nixpkgs/NixOS` channels correspond with `nixpkgs` repository branches (or vice versa).
So `origin/nixos-17.03` contains the built/tested version of the derivations for 17.03.

You can always tell which version of `nixpkgs` you are linking to:

```
    nix-instantiate --eval --expr ''builtins.readFile <nixpkgs/.version>''
```

To join the fun you can clone the nixpkgs from github and start modifying expressions to your
hearts content. You can then load an environment against your version of `<nixpkgs>`. This
is a great way to start expanding and debugging the package sets and joining the community.

```
    nix-shell -I nixpkgs=~/my-local-clone-of-nixpkgs
```', '', '2021-02-15 13:46:14.95258+00', '1858-11-17', 'americano');
INSERT INTO public.nix_coffees (id, title, body, labels, created_at, last_drank, coffee_type) VALUES ('c70d1504-4d69-46a2-a78b-a09ac22cd3c7', 'Frozen Channels', '`nix-channels` has its flaws too. The problem is essentially that you are
building against a moving target. This is fine if you are in free flow hacking mode
but if you require enterprise stability you probably want to freeze the `<nixpkgs>`
version at a certain point for each minor or major release of your project.
It''s a little bit of a lucky dip whether you freeze on a version of `nixpkgs`
that is "a happy one" as in your users are untroubled or a "difficult one".
The Rustaceans seem to have a sensible nightly-build -> version system that
is worth looking at the [train model](https://doc.rust-lang.org/book/appendix-07-nightly-rust.html).

When we come to flakes we will see another potential variation which shares some similarities with
some of the rust ideas.', '', '2021-02-15 14:33:51.923009+00', '1858-11-17', 'americano');
INSERT INTO public.nix_coffees (id, title, body, labels, created_at, last_drank, coffee_type) VALUES ('a78eea2a-d52c-4fe0-9651-4e26b04d4867', 'Nix: Garbage Collection Services Run On Tuesday', '[Nix Garbage Collection](https://nixos.org/guides/nix-pills/garbage-collector.html)
Here''s a quick recipe:

```
    nix-channel --update (descriptions (derivations) of all software)
    nix-env -u --always (upgrade installed packages)
    rm /nix/var/nix/gcroots/auto/*
    nix-collect-garbage -d
```

[Garbage Collection](https://nixos.org/guides/nix-pills/garbage-collector.html)
Go have a look through `/nix/var/nix/gcroots/`. These are the garbage collection roots.
Whatever they point to and say "I still need that" should be protected in a garbage
collection cycle.', '', '2021-02-15 14:47:12.89937+00', '1858-11-17', 'americano');
INSERT INTO public.nix_coffees (id, title, body, labels, created_at, last_drank, coffee_type) VALUES ('74e7162d-c9b7-4c8f-a06b-7a5725c637af', 'Nix(snow)Flakes', 'This is one of those big changes in the country of nix that is on the horizon.
The flakes aim to improve certain aspects of nix:
From Dolstra (RFC 49):

```
Abstract: This RFC proposes a mechanism to package Nix expressions into composable
entities called "flakes". Flakes allow hermetic, reproducible evaluation of
multi-repository Nix projects; impose a discoverable, standard structure on Nix projects;
and replace previous mechanisms such as Nix channels and the Nix search path.
```

A series of developer posts describing flakes can be found here:
[TweagIO Flakes](https://www.tweag.io/blog/2020-05-25-flakes/).

Notes from Alexander Bentyev on getting started through his
[NixCon](https://github.com/serokell/nixcon2020-talk).

> Everything is a flake... nixpkgs, Nix, that
  serokell presentation, if there is a flake.nix
  in the root directory it is a flake.

See also:

[Dolstra gist](https://gist.github.com/edolstra/40da6e3a4d4ee8fd019395365e0772e7)

[Zimbatm](https://zimbatm.com/NixFlakes/)

[How to do a flake build in non-nixos system?](https://discourse.nixos.org/t/how-to-do-a-flake-build-in-non-nixos-system/10450/13)', '', '2021-02-15 14:48:17.91632+00', '1858-11-17', 'americano');
INSERT INTO public.nix_coffees (id, title, body, labels, created_at, last_drank, coffee_type) VALUES ('f903d496-1184-4899-990b-f879e7ae43de', 'Overlays', 'Overlays are popular. People who seem to understand `nix` speak about
them approvingly.

Here is an example from Eelco himself example:

```
nixpkgs.overlays = [(self: super: {
    hostapd = super.hostapd.overrideDerivation(prev: {
        pathches = prev.patches or []
            ++ [./no-scan.patch];
    });
})]
```

[The dos and donts of overlays](https://blog.flyingcircus.io/2017/11/07/nixos-the-dos-and-donts-of-nixpkgs-overlays/)

[Manual Overlays](https://nixos.org/manual/nixpkgs/stable/#chap-overlays)

[NixCon Live Stream](https://www.youtube.com/watch?t=39m50s&v=6bLF7zqB7EM&feature=youtu.be)', '', '2021-02-15 14:49:43.248729+00', '1858-11-17', 'americano');
INSERT INTO public.nix_coffees (id, title, body, labels, created_at, last_drank, coffee_type) VALUES ('9b53c551-fa77-4b21-bc9a-3b8efd2b2e4d', 'Apple''s Standard Environment', '> The stdenv is not a special derivation to Nix, but it''s very important
  for the nixpkgs repository. It serves as base for packaging software.
  It is used to pull in dependencies such as the GCC toolchain, GNU make,
  core utilities, patch and diff utilities, and so on. Basic tools
  needed to compile a huge pile of software currently present in nixpkgs.


```
[nix-shell:~/ihp-nix]$ nix repl ''<nixpkgs>''
Welcome to Nix version 2.4pre20201201_5a6ddb3. Type :? for help.

Loading ''<nixpkgs>''...
Added 12822 variables.

nix-repl> stdenv
«derivation /nix/store/ba47l0vmz06jxyflpz0ziqz009d5qp01-stdenv-darwin.drv»

nix-repl> stdenv.
stdenv.__bootPackages            stdenv.drvAttrs                  stdenv.isDarwin                  stdenv.out
stdenv.__extraImpureHostDeps     stdenv.drvPath                   stdenv.isFreeBSD                 stdenv.outPath
stdenv.__hatPackages             stdenv.extraBuildInputs          stdenv.isLinux                   stdenv.outputName
stdenv.__impureHostDeps          stdenv.extraNativeBuildInputs    stdenv.isMips                    stdenv.override
stdenv.__sandboxProfile          stdenv.extraSandboxProfile       stdenv.isOpenBSD                 stdenv.overrideDerivation
stdenv.all                       stdenv.fetchurlBoot              stdenv.isSunOS                   stdenv.overrides
stdenv.allowedRequisites         stdenv.hasCC                     stdenv.isi686                    stdenv.platform
stdenv.appleSdkVersion           stdenv.hostPlatform              stdenv.isx86_32                  stdenv.preHook
stdenv.args                      stdenv.initialPath               stdenv.isx86_64                  stdenv.setup
stdenv.bootstrapTools            stdenv.is32bit                   stdenv.lib                       stdenv.shell
stdenv.buildPlatform             stdenv.is64bit                   stdenv.libc                      stdenv.shellPackage
stdenv.builder                   stdenv.isAarch32                 stdenv.macosVersionMin           stdenv.system
stdenv.cc                        stdenv.isAarch64                 stdenv.meta                      stdenv.targetPlatform
stdenv.defaultBuildInputs        stdenv.isBigEndian               stdenv.mkDerivation              stdenv.type

nix-repl> stdenv.appleSdkVersion
"10.12"
```', '', '2021-02-15 14:51:54.865263+00', '1858-11-17', 'americano');
INSERT INTO public.nix_coffees (id, title, body, labels, created_at, last_drank, coffee_type) VALUES ('ac82ca5d-479b-4278-98aa-441cb9d39572', 'Big Sur', '```
New in macOS Big Sur 11.0.1, the system ships with a built-in dynamic linker cache of all system-provided libraries.
As part of this change, copies of dynamic libraries are no longer present on the filesystem.
Code that attempts to check for dynamic library presence by looking for a file at a path or
enumerating a directory will fail. Instead, check for library presence by attempting to
dlopen() the path, which will correctly check for the library in the cache. (62986286)

This means that we no longer ship individual dylib files on disk. You should try
passing in the -isysroot <PATH_TO_MACOS_SDK> flag to your clang link invocation.
```

Here''s a good example of the kind of work to be done managing Darwin changes
with `<nixpkgs>`:

[impossible to link (ld / clang) liboping on macOS Big Sur 11](https://github.com/NixOS/nixpkgs/issues/91748#issuecomment-727641022)

The big solution is to update the expressions so that they can run with the new placement of Apple''s runtime libraries.

    [darwin (Big Sur): tbd based stdenv #98541](https://github.com/NixOS/nixpkgs/pull/98541)

    [darwin: Big Sur-compatible stdenv bootstrap #107013](https://github.com/NixOS/nixpkgs/pull/107013)

    [the fixes](https://gist.github.com/LnL7/a17a042dd21258b957279637bb6be627)

See also:

[Nixpkgs macOS Stdenv Updates](https://matthewbauer.us/blog/darwin-stdenv-update.html)

for more background.

Apple/darwin/Mac is and always will be a moving target:

[Apple cctools and ld64 port for Linux, BSD and macOS](https://github.com/tpoechtrager/cctools-port)

[big sur release notes](https://developer.apple.com/documentation/macos-release-notes/macos-big-sur-11_1-beta-release-notes)

[big sur release notes](https://developer.apple.com/documentation/macos-release-notes/macos-big-sur-11_0_1-release-notes)

The important change is:

[macos big sure issue](https://github.com/NixOS/nixpkgs/issues/104580)
[nix bootstrap macos](https://discourse.nixos.org/t/is-there-a-plan-that-nix-can-bootstrap-itself-on-macos-big-sur/10043/2)
[macos and clang](https://stackoverflow.com/questions/51161225/how-can-i-make-macos-frameworks-available-to-clang-in-a-nix-environment)

Many Mac/Nix issues boil down to just being able to locate the right name.
`/System/Library/Frameworks/CoreFoundation.framework/Versions/A/CoreFoundation`

```
    _OBJC_CLASS_$_NSArray is undefined
```

If you are just missing `\_NS\*` symbols you just need `-framework CoreFoundation` 

Also of interest is using the NixOS [virtual machine](https://github.com/NixOS/nixpkgs/issues/108984).
', '', '2021-02-15 15:00:54.955115+00', '1858-11-17', 'americano');
INSERT INTO public.nix_coffees (id, title, body, labels, created_at, last_drank, coffee_type) VALUES ('f9f28eb6-2586-4b7e-b70b-9a463cca2e36', 'Apple''s Made From Silicon', '[What''s Apple Silicon?](https://en.wikipedia.org/wiki/Mac_transition_to_Apple_Silicon)

Transition from `x86_64` to the `ARM64` chip architecture.

One of the big things to get ready for is code
signing on [Support for Apple Silicon (aarch64-darwin)](https://github.com/NixOS/nixpkgs/issues/95903)

[Can''t you just right click?](https://lapcatsoftware.com/articles/unsigned.html)

[Apple Silicon Macs Will Require Code Signing](https://eclecticlight.co/2020/08/22/apple-silicon-macs-will-require-signed-code/)

[targeting ARM with nix](https://www.gitmemory.com/issue/NixOS/nixpkgs/95903/730039057)

[Brew Community Has Similar Consideration](https://github.com/Homebrew/brew/issues/7857)

[sigtool](https://github.com/thefloweringash/sigtool)', '', '2021-02-15 15:02:15.994382+00', '1858-11-17', 'americano');
INSERT INTO public.nix_coffees (id, title, body, labels, created_at, last_drank, coffee_type) VALUES ('c7d2879c-c73a-48fb-bdb5-70a0523110e5', 'Rusty Nix', '[Packaging (Nightly) Rust Application with Nix](https://www.breakds.org/post/build-rust-package/)

[Carnix](https://discourse.nixos.org/t/troubleshooting-a-rust-build-with-carnix-on-a-macos-system/3950)

Here are some rust issues that arose on darwin.

[nix-daemon failing on SSL CA](https://github.com/NixOS/nix/compare/51a4a5e1cf3b...a39798b64ca4)

[rustPlatform targe llvmPackages5 when using clang](https://github.com/NixOS/nixpkgs/compare/5d0e2dedd559...c43e01b6d282)', '', '2021-02-15 15:04:43.411806+00', '1858-11-17', 'americano');
INSERT INTO public.nix_coffees (id, title, body, labels, created_at, last_drank, coffee_type) VALUES ('762c138a-b8ab-44dc-a365-af195547de7b', 'The Ultimate Source Of Truth: The Stored Program Computer', 'If you feel lost in a world of `nix` and darwin operating systems, unsure of what is
pure and impure remember this always:

```
    Your cpu will only execute streams of instructions and these instructions
    are stored as numbers somewhere in your computers memory.
```

Those instructions are stored somewhere! So all you need to do is find out where they are
and how to get your code to point them.

If all seems at a loss with building in darwin remember this principle!
There is always a way to build, patch, overlay, and find all the binaries you need. And
nix will help you do it.', '', '2021-02-15 15:03:35.428775+00', '1800-11-01', 'americano');


ALTER TABLE public.nix_coffees ENABLE TRIGGER ALL;


