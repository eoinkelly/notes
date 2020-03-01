# GraphQL

Sources

* https://graphql.org/learn/queries/

## Questions

what is the deal with nodes and edges as key names?

## Overview

> A GraphQL service is created by defining types and fields on those types,
> then providing functions for each field on each type.


In GraphQL, every field and nested object can get its own set of arguments, making GraphQL a complete replacement for making multiple API fetches. You can even pass arguments into scalar fields, to implement data transformations once on the server, instead of on every client separately

## Operations

The operation type is either

1. query
    * can be run in parallel
1. mutation
    * run in series
1. subscription

The operation type is required unless you're using the query shorthand syntax,
in which case you can't supply a name or variable definitions for your
operation.

## Short-hand syntax

* you can use short-hand if your operation is `query`
* names are required in a multi-operation document

```
# long hand
query <QUERY_NAME>(<QUERY_ARGS>) {
  ...
}

# shorthand (cannot add args)
{
  ...
}
```

## Fragements

```
query ThQuery {
  ...<FRAGMENT_NAME> # this is how a fragment is referenced

  ... on <TYPENAME> { # this is how an inline fragment is referenced
  }
}

fragment <FRAGMENT_NAME> on <TYPENAME> {
  <FIELDS>
}
```

* can be declared with a name or declared inline
* fragments can reference variables by `$varname` - the variables are defined in the query/mutation/subscription definiton

## Variables

* graphql lets you pass variables separately (similar idea to how the Postgres protocol allows)
* you pass a dictionary (JSON object ish) of variables
* they want to avoid doing string interpolation on the client
* make variables required with !
* variables can have default values
* variables must be on eof
    1. scalar
    1. enum
    1. "input object type"

# Directives

* `fieldName @include(if: $booleanVar) `
* `fieldName @skip(if: $booleanVar) `
```
query Hero($episode: Episode, $withFriends: Boolean!) {
  hero(episode: $episode) {
    name
    friends @include(if: $withFriends) {
      name
    }
  }
}
```

## Types

* types have a _kind_
* the kind decides what fields area available to describe that type
    * scalars have
        1. name
        1. description
    * enums
        1. name
        1. description
        1. the values of the enum
    * Objects
        1. name
        1. description
        1. the fields in the object
    * Interfaces
        1. name
        1. description
        1. the fields in the object
        1. the concrete object types possible at runtime
    * Union types
        1. the concrete object types possible at runtime
    * Lists
        * ???
    * NonNull
        * ???
    * InputObject

    SCALAR
    Indicates this type is a scalar.

    OBJECT
    Indicates this type is an object. fields and interfaces are valid fields.

    INTERFACE
    Indicates this type is an interface. fields and possibleTypes are valid fields.

    UNION
    Indicates this type is a union. possibleTypes is a valid field.

    ENUM
    Indicates this type is an enum. enumValues is a valid field.

    INPUT_OBJECT
    Indicates this type is an input object. inputFields is a valid field.

    LIST
    Indicates this type is a list. ofType is a valid field.

    NON_NULL
    Indicates this type is a non-null. ofType is a valid field.

`__TypeKind` enum

```
query MyQuery {
  qtype: __type(name: "Query") {
    name
    __typename
    description
    fields {
      name
      __typename
      description
      deprecationReason
    }
    interfaces {
      name
      __typename
      description
    }
    kind
  }
  ccType: __type(name: "codesOfConduct") {
    name
    __typename
    description
  }
  mtype: __type(name: "Mutation") {
    name
    description
    kind
  }
}
```

## Interfaces

* is an abstract type
* a common set of fields that can be part of multiple types


## Union types

* is an abstract type
* A type which represents `A | B`

```
query HeroForEpisode($ep: Episode!) {
  hero(episode: $ep) {
    name
    ... on Droid {
      primaryFunction
    }
    ... on Human {
      height
    }
  }
}
```

## Meta fields

* graphql has a few metadata files you can use to introspect the objects you get back from the server

`__typename`
