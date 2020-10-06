recordToInputValue :: HasField "id" entity (Id entity) => entity -> Text 

recordToInputValue :: (HasField "id" entity (Id entity)) => entity -> Text
recordToInputValue entity =
    getField @"id" entity
    |> Newtype.unpack
    |> Data.UUID.toText
{-# INLINE recordToInputValue #-}
