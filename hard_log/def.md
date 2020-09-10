 ihp/IHP/ModelSupport.hs 
-------------------------
There is a Haskell module called Data.Default which defines

class Default a where
#

Here are some IHP Default instances:
```
instance Default Text where
   {-# INLINE def #-}
   def = "" 
```

```
instance Default Bool where
    {-# INLINE def #-}
    def = False
```

and a slightly fancier default

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
