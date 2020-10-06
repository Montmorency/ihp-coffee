The `fill` function takes care of populating multiple fields in a record at once. In your controller logic you will often have helper functions like:

```
buildPost post = post
    |> fill @'["title", "body"]
```
which will grab the `title` and `body` param keys from a request and populate the `post` record with the param values. This is all straight from the IHP docs but let us make this recycled Espresso a Lungo by looking at the code. The `fill` function is actually provided by the `FillParams` class: 

```
class FillParams (params ::[Symbol]) record where
      fill :: (
           ?requestContext :: RequestContext
           , HasField "meta" record ModelSupport.MetaBag
           , SetField "meta" record ModelSupport.MetaBag
           ) => record -> record
```

`FillParams` accepts two type parameters: a list of type level names and a record. It also expects the `?requestContext` implicit parameter which is available in all IHP actions, and, finally, the constraints require our record has a `meta` field. In IHP the `meta` record field holds a `MetaBag` data type. This bag gives IHP a way of tracking problems with a form. A user might get most of the form "right" like provide a name, an address, etc., but they may not give a valid email address. The `MetaBag` contains the 'annotations'; basically, a place to stores notes on what went wrong (or what has been changed) so that the form can be passed back to the user with the correct fields still correct and the problematic fields highlighted. Have a look through `IHP.Controller.Param` for more details!

