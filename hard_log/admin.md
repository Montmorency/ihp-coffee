Learning how IHP works has the added benefit of making 
some of our ideas about Haskell more concrete. 
Let us look at how `LoginSupport` works in IHP.
In the process we will see an example of the subtle
difference between a `type` synonym and a `type instance` synonym.

Firstly where does the `Admin` data type come from? 
It came from you! You (should have) 
built that record into your Application's Postgres Schema:
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
IHP generated all the Haskell side types (with its clever tricks for pluralising names,
CamelCasing data etc.). It went to the trouble of taking `TABLE admins` 
and generated you an `Admin` data type.
Have a look in ./build/Generated/Types.hs` to see what `instance` "armour" is placed on `Admin`.
As you can see in `Generated/Types.hs` IHP equipped
your data to the teeth with useful instances 
for going and doing battle on the internet. 

Now in `Application/Helper/Controller.hs` we should have defined something like 
`type instance CurrentAdminRecord = Admin`. In other words we make a type instance 
synonym called `CurrentAdminRecord`for our Admin data type. It is a type instance 
declaration because it a synonym for a `type family CurrentAdminRecord` 
(defined in LoginSupport/Helper/Controller.hs)
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
is coming from a valid Administrator and, if it doesn't, it will redirect to the Admin login
page if it is not. 

These kind of patterns make factoring permission based Views in your IHP application 
simple to implement and secure.
