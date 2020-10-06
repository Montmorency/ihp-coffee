application ~ ViewApp viewContext

 A type context can include equality constraints of the form t1 ~ t2, which denote that the types t1 and t2 need to be the same. In the presence of type families, whether two types are equal cannot generally be decided locally. Hence, the contexts of function signatures may include equality constraints, as in the following example: 
