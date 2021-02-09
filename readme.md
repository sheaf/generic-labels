# generic-labels <a href="https://hackage.haskell.org/package/generic-labels" alt="Hackage"><img src="https://img.shields.io/hackage/v/generic-labels.svg" /></a>

* [Disclaimer](#disclaimer)
* [Introduction](#introduction)
* [Usage](#usage)
  - [Adapters](#adapters)
  - [Projections](#projections)
  - [Injections](#injections)
* [Comparison with `generic-lens`](#comparison)

<a name="disclaimer"></a>
# Disclaimer

This is a little experiment on a variant of [`generic-lens`](https://hackage.haskell.org/package/generic-lens)'s [structural subtyping](https://hackage.haskell.org/package/generic-lens/docs/Data-Generics-Product-Subtype.html)
with support for labelled types (of the form `"label" := ty`) in tandem with record field names.

I've tried to ensure reasonable error messages and type inference, but its use should probably remain limited
to simple situations like flat records, similarly to [`generic-lens`](https://hackage.haskell.org/package/generic-lens)'s [`Subtype`](https://hackage.haskell.org/package/generic-lens/docs/Data-Generics-Product-Subtype.html).

<a name="introduction"></a>
# Introduction

This library performs impedance matching for collections of labelled types,
similar to the [`Subtype`](https://hackage.haskell.org/package/generic-lens/docs/Data-Generics-Product-Subtype.html)
functionality of [`generic-lens`](https://hackage.haskell.org/package/generic-lens).

This allows one to project out a collection of fields of a record, to plug a smaller record into a bigger one, and to
build up a record out of two sub-parts.

It can also be useful to emulate very basic extensible records, as the library supports both built-in record field names
as well as explicitly labelled types using [`OverloadedLabels`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_labels.html).

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

We then have a function `foo :: AllArgs -> res` that we might want to call on only a subset of the fields of `AllArgs`. We can define:

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
call2 = adaptedFoo ( #f2 := 1.0, #b := True, #f1 := 0.5 )
call3 = adaptedFoo ( #f1 := 0.5, #f2 := 1.0, #f3 := -1.0, #f4 := 2.0 )
```

Invalid uses will throw relevant error messages, for instance:

```haskell
call4 :: res
call4 = adaptedFoo ( #f1 := 0.5, #b := True )
```

```
  * No instance for
        Adapt
          ("f1" := Float, "b" := Bool)
          ("f3" := Float, "f4" := Float, "b" := Bool)
          ("f1" := Float, "f2" := Float, "f3" := Float, "f4" := Float, "b" := Bool)

    The following types are non-optional but have not been provided:
      - #f2 := Float
```

```haskell
call5 :: res
call5 = adaptedFoo ( #f1 := 0.5, #f2 := 1.0, #xxx := "redundant" )
```

```
  * No instance for
        Adapt
          ("f1" := Float, "f2" := Float, "xxx" := [Char])
          ("f3" := Float, "f4" := Float, "b" := Bool)
          ("f1" := Float, "f2" := Float, "f3" := Float, "f4" := Float, "b" := Bool)

    The following provided types do not appear in the destination:
      - #xxx := [Char]
```

These examples have used explicitly named arguments using `OverloadedLabels`, but plain records are also supported:

```haskell
data FI = FI { float :: Float, int :: Int }
  deriving stock Generic

data CIBF = CIBF
  { char  :: Char
  , int   :: Int
  , bool  :: Bool
  , float :: Float
  }
  deriving stock Generic

data BC = BC { bool :: Bool, char :: Char }
  deriving stock Generic

fi_to_cibf :: FI -> CIBF
fi_to_cibf fi = adapt fi ( BC { bool = False, char = '?' } )
```

<a name="projections"></a>
## Projections

We can project out a subset of fields, similarly to the [`upcast`](https://hackage.haskell.org/package/generic-lens/docs/Data-Generics-Product-Subtype.html#v:upcast)
function from [`generic-lens`](https://hackage.haskell.org/package/generic-lens).

```haskell
project :: Project big small => big -> small
```

Re-using the example from the previous section, we can project out just the `"bool"` and `"char"` fields:

```haskell
cibf_to_bc :: CIBF -> BC
cibf_to_bc = project
```


<a name="injections"></a>
## Injections

We can override a subset of the fields of a record with those from a smaller record,
similarly to the [`smash`](https://hackage.haskell.org/package/generic-lens/docs/Data-Generics-Product-Subtype.html#v:smash)
function from [`generic-lens`](https://hackage.haskell.org/package/generic-lens):

```haskell
inject :: Inject small big => small -> big -> big
```

Re-using the running example:

```haskell
plug_in_bc :: BC -> CIBF -> CIBF
plug_in_bc = inject
```

<a name="comparison"></a>
# Comparison with `generic-lens`

This library is based off [`generic-lens`](https://hackage.haskell.org/package/generic-lens), with a modification to the underlying framework
of its [`Subtype`](https://hackage.haskell.org/package/generic-lens/docs/Data-Generics-Product-Subtype.html) typeclass in order
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

Another difference is that this library provides the more general function `adapt`, which allows us to
build up a record out of two separate parts (useful for emulating named optional arguments).    
This is in contrast to the [`super`](https://hackage.haskell.org/package/generic-lens/docs/Data-Generics-Product-Subtype.html#v:super)
lens, which only allows us to focus on a single subpart – with no notion of its complement.
