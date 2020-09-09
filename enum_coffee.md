Postgres ENUM and IHP Selection Fields
----------------------------------------

To get the IHP to do the form rendering work 
we need to make our ENUM model an instance of Can Select

selectField (...) => Proxy fieldName -> [item] -> FormField

The items are the ENUM data constructors, you can have a look at those in 
the `Generated/Types.hs`.

they have some constraints on them namely 

    CanSelect item
    InputeValue (SelectValue item)

```
instance CanSelect Coffeetype where
    type SelectValue Coffeetype = Coffeetype
    selectValue value = value 
    selectLabel = tshow
```

`tshow` is short hand here that saves us from setting all the individual
label names.  

[See The PostGres Docs on ENUM](https://www.postgresql.org/docs/9.1/datatype-enum.html)
----------------------------------------------------------------------------------------
A few highlights are:

An enum value occupies four bytes on disk. The length of an 
enum value's textual label is limited by the 
NAMEDATALEN setting compiled into PostgreSQL; 
in standard builds this means at most 63 bytes.

Enum labels are case sensitive, so 'happy' 
is not the same as 'HAPPY'. White space in the 
labels is significant too.

The translations from internal enum values to 
textual labels are kept in the system catalog pg_enum. 
Querying this catalog directly can be useful.
