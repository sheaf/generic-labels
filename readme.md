# generic-labels <a href="https://hackage.haskell.org/package/generic-labels" alt="Hackage"><img src="https://img.shields.io/hackage/v/generic-labels.svg" /></a>

* [Introduction](#introduction)
* [Usage](#usage)
  - [Adapters](#adapters)
  - [Projections](#projections)
  - [Injections](#injections)
* [Comparison with existing libraries](#comparison)

<a name="introduction"></a>
# Introduction

This library performs impedance matching for collections of labelled types,
similar to the [`Subtype`](https://hackage.haskell.org/package/generic-lens-2.1.0.0/docs/Data-Generics-Product-Subtype.html)
functionality of [`generic-lens`](https://hackage.haskell.org/package/generic-lens).

This allows one to project out a collection of fields of a record, or to plug a smaller record into a bigger one.

It can also be useful to emulate simple extensible records, as the library supports both built-in record field names
as well as explicitly labelled types using `OverloadedLabels`.

<a name="usage"></a>
# Usage

<a name="adapters"></a>
## Adapters

The main function this library exports is

```haskell
adapt :: Adapt args opt all => args -> opt -> all
```

Here `adapt` can be thought of as an adapter for a function which requires a certain collection of arguments,
but also provides default values.

For instance, consider a function which takes:
  - two (required) `Float`s,
  - one optional `Float` with default value `0`,
  - one optional `Float` with default value `1`,
  - one optional `Bool` with default value `False`.

This corresponds to the following types:

```haskell
type AllArgs = ( "f1" := Float, "f2" := Float, "f3" := Float, "f4" := Float, "b" := Bool )
type OptArgs = ( "f3" := Float, "f4" := Float, "b" := Bool )
```

We then have a function `foo :: AllArgs -> res`

that we might want to call on only a subset of the fields of `AllArgs`. We can define:

```haskell
adaptedFoo :: Adapt args OptArgs AllArgs => args -> res
adaptedFoo args = foo ( adapt args defaults )
  where
  defaults :: OptArgs
  defaults = ( #f3 := 0, #f4 := 1, #b := False )
```

We can then call `adaptedFoo` at any collection of fields, provided that the required arguments are present:


```haskell
call1, call2, call3 :: res
call1 = adaptedFoo ( #f1 := 0.5, #f2 := 1.0 )
call2 = adaptedFoo ( #f2 := 1.0, #b = True, #f1 := 0.5 )
call3 = adaptedFoo ( #f1 := 0.5, #f2 := 1.0, #f3 := -1.0, #f4 := 2.0 )
```

These examples have used explicitly named arguments using `OverloadedLabels`, but plain records are also supported:

```haskell
data IF = IF { int :: Int, float :: Float }
  deriving stock Generic

data CFBI = CFBI
  { char  :: Char
  , float :: Float
  , bool  :: Bool
  , int   :: Int
  }
  deriving stock Generic

data BC = BC { bool :: Bool, char :: Char }
  deriving stock Generic

if_to_cfbi :: IFC -> CFBI
if_to_cfbi ifc = adapt ifc ( BC { bool = False, char = '?' } )
```

<a name="projections"></a>
## Projections

We can project out a subset of fields, similarly to the [`upcast`](https://hackage.haskell.org/package/generic-lens-2.1.0.0/docs/Data-Generics-Product-Subtype.html#v:upcast)
function from [`generic-lens`](https://hackage.haskell.org/package/generic-lens).

```haskell
project :: Project big small => big small
```

Re-using the example from the previous section, we can project out just the `"bool"` and `"char"` fields:

```haskell
cfbi_to_bc :: CFBI -> BC
cfbi_to_bc = project
```


<a name="injections"></a>
## Injections

We can override a subset of the fields from a larger record, similarly to the [`smash`](https://hackage.haskell.org/package/generic-lens-2.1.0.0/docs/Data-Generics-Product-Subtype.html#v:smash)
function from [`generic-lens`](https://hackage.haskell.org/package/generic-lens).


Re-using the running example:

```haskell
plug_in_bc :: BC -> CFBI -> CFBI
plug_in_bc = inject
```

<a name="comparison"></a>
# Comparison with existing libraries

This library is based off [`generic-lens`](https://hackage.haskell.org/package/generic-lens), with a modification to the underlying framework for
the [`Subtype`](https://hackage.haskell.org/package/generic-lens-2.1.0.0/docs/Data-Generics-Product-Subtype.html) typeclass in order
to support both standard record field names as well as explicitly labelled types.

This allows us to write code like the following:

```haskell
data IBC = IBC
  { int  :: Int
  , bool :: Bool
  , char :: Char
  }
  deriving stock Generic

ibc_to_ci :: IBC -> ( "char" := Char, "bool" := Bool )
ibc_to_ci = project
```

We can thus re-use anonymous tuple types, providing a basic version of extensible records.
