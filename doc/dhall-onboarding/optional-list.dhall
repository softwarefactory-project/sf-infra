{-
$ dhall-to-yaml --file optional-list.dhall

Error: Wrong type of function argument

- Type
+ … → …

59│                                         Optional List


# What is Optional
Dhall says that a function except a `Type` and we give it a `Function`
(… → … means a function that goes from any types to any other types)

The affected function is `Optional`. It is a type constructor,
You can get it's signature by running:

  dhall --ascii type <<< "Optional"
  Type -> Type

In other words, given a `Type`, the Optional function creates a new type.


# Function call in dhall
Dhall doesn't uses parenthesis to describe function call.
In Python:

  Optional(Text)

Is written:

  Optional Text


And to write (in python):

  Optional(List Text)

You need to use parenthesis to force the argument grouping:

  Optional (List Text)


# What is None
The same problems occurs on the next line,
`None` is function to create an Optional empty value:

  dhall --ascii type <<< "None"
  forall (A : Type) -> Optional A

To create an empty Optional list of text, you need to use parenthesis too:

  None (List Text)

-}
let Server =
      { Type = { name : Text, volumes : Optional List Text }
      , default.volumes = [] : None List Text
      }

in  Server::{ name = "test" }
