# Usage guide

Morello 🍒 is a library that helps you validate and convert data from one format into another. It is particularly well-suited for transforming a weakly-typed input format - think of a Json input - into a strongly-typed output format - think of your well-typed internal domain model. However you can also use it for converting between well-typed models.

Let's start with a simple example. Suppose we are building a microservice for managing products of an ecommerce shop and get data from an import service. 

Our initial input model is defined as

```purescript
type InputProduct = {
    productName :: String 
}
```
and we want to convert it into our "proper" domain model

```purescript
newtype ProductName = ProductName String 

type Product = {
    name :: ProductName
}
```
A naive conversion function could look like this:

```purescript
convert :: InputProduct -> Product
```

But let's be honest, you can never trust your input data. So we probably also  want to validate it during conversion. For our example, let's just say that a product name should have at least 5 letters. 

So we want a conversion that also validates:

```purecript
convert :: InputProduct -> Validated Product
```

`Validated` is basically like an `Either`, indicating that either the validation failed - the `Left` case - or we got a valid product - the `Right` case. But more on this later.

Morello helps us write this function. In morello, every conversion starts with a branch 🌱...

```purescript 
convert :: InputProduct -> Validated Product
convert = branch >>> ?h 
```
... and continues with one (or more) cherries 🍒...

```purescript 
convert :: InputProduct -> Validated Product
convert = branch 
            >>> cherry ?h
```

... and finishes with a blossom 🌸 

```purescript 
convert :: InputProduct -> Validated Product
convert = branch 
            >>> cherry ?h
            >>> blossom 
```

`cherry` is where all the magic happens, so let's fill it with life! In `cherry` we define how our output data should look like. In our simple example, this is a record with a field `name` (we'll see more exciting examples in a minute, bear with me!):

```purescript 
convert :: InputProduct -> Validated Product
convert = branch 
            >>> cherry {
                name : ?h
            }
            >>> blossom 
```

Now we basically need to answer the two quesitons:
1. Where does the value come from?
2. How do we validate it?

Morello works with [lenses](https://github.com/purescript-contrib/purescript-profunctor-lenses). If you don't yet know what a lens is, just think of it as kind of a (very powerful) *getter* - a way to get data out of a record. For our example, we'll define a simple lens that gets the field `productName` out of a record:

```purescript
nameL = prop (Proxy :: Proxy "productName")
```

Don't be scared of the `Proxy` part here, it just tells the compiler that it's about the field `productName` (we'll see another, simpler way to create a lens later).

For the validation bit we create a `Validate String ProductName`, which is just a type alias for a function from `String` to `Validated ProductName`: 

```purescript 
validateName :: Validate String ProductName
validateName name = 
    if length name > 5 then 
        valid (ProductName name)
    else
        invalid (FieldInvalid "Product name is too short.")
```
Now we got all the pieces to define how to `pick` data from our input and validate it:

```purescript 
convert :: InputProduct -> Validated Product
convert = branch 
            >>> cherry {
                name : 
                    pick (nameL) validateName :: Pick InputProduct ProductName
            }
            >>> blossom 
```
