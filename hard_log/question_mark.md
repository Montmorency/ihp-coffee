 ?
---
All variables starting with ? are implicit parameters that are 
passed around without explicit adding them to function signatures. For instance the 
RequestContext is defined as:

```
data RequestContext = RequestContext
    { request :: Request
    , respond :: Respond
    , params :: [Param]
    , files :: [File LBS.ByteString]
    , vault :: (Vault.Key (Session IO String String))
    }
```


(?requestContext::RequestContext) is just a small variable holding references 
to the wai request, request parameters, file uploads etc.
You may also come across `?controllerContext :: ControllerContext`
and `?theAction :: action`.

Have a look in IHP/ControllerSupport.hs for the definitions.

```
newtype ControllerContext = ControllerContext TypeMap.TMap
newtype ActionType = ActionType Typeable.TypeRep
```

`TypeMap.TMap` is essentially a dictionary with every key 
having the same type as its value. Have a look at the
Typeable Haskell module to learn more about ActionType.
