---
title: Developing and packaging software in Haskell
author: Andreas Abel
date: 3 March 2025
---

This lecture gives a brief introduction to developing software in
Haskell, covering packaging, versioning, continuous integration, and
package repositories.

The code for this lecture can be found in repository https://github.com/teach-afp/binary-search-trees .


Basic structure of a Haskell project
====================================

Running example: a search tree library
--------------------------------------

As a running example, we consider a toy implementation of sets as binary search trees (without balancing).
We aim to package and publish this as library `binary-search-tree`.

The code is structured as follows:

 - `src/Data/BST.hs`: interface, hiding implementation

   ```haskell
   type BST a

   -- Construction
   empty     ::          BST a
   singleton ::          a -> BST a
   insert    :: Ord a => a -> BST a -> BST a
   fromList  :: Ord a => [a] -> BST a
   union     :: Ord a => BST a -> BST a -> BST a

   instance     Ord a => Semigroup (BST a)
   instance     Ord a => Monoid    (BST a)

   -- Deletion
   delete    :: Ord a => a -> BST a -> BST a
   split     :: Ord a => a -> BST a -> (BST a, BST a)

   -- Query
   member    :: Ord a => a -> BST a -> Bool

   instance              Foldable BST
   ```

 - `src/Data/BST/Internal.hs`: implementation, exporting everything

   ```haskell
   data BST a
     = Leaf
     | Node (BST a) a (BST a)
   ```

It is good practice to also publish the implementation so that users can implement missing functionality efficiently.
E.g., maybe you are missing a function `lookupMax :: Ord a => BST a -> Maybe a` that you can only implement efficiently if you have access to the constructors of `BST`.

However, only the interface is officially public and thus binding.

Basic packaging
---------------

Haskell software is usually packaged with [Cabal].
While [Stack] is another build system for Haskell, it also uses `.cabal` files to define packages.

[Cabal]: https://cabal.readthedocs.io/en/stable/

The following file `binary-search-tree.cabal` is our first take on a Cabal package description for our library:

```cabal
cabal-version: 2.2
name: binary-search-tree
version: 0.0.0.0

common common-build-parameters
  default-language:
    Haskell2010
  default-extensions:
    LambdaCase
    InstanceSigs
  ghc-options:
    -Wall
    -Wcompat

library
  import: common-build-parameters

  build-depends:
    , base

  hs-source-dirs:
    src

  exposed-modules:
    Data.BST
  other-modules:
    Data.BST.Internal
```

It contains of tree mandatory _fields_ [cabal-version], [name], and [version] and two _stanzas_, a [common] stanza and a [library] stanza, each of which contains again a couple of fields.

[cabal-version]:     https://cabal.readthedocs.io/en/3.14/file-format-changelog.html
[cabal-version 2.2]: https://cabal.readthedocs.io/en/3.14/file-format-changelog.html#cabal-version-2-2
[name]:              https://cabal.readthedocs.io/en/3.14/cabal-package-description-file.html#pkg-field-name
[version]:           https://cabal.readthedocs.io/en/3.14/cabal-package-description-file.html#pkg-field-version
[common]:            https://cabal.readthedocs.io/en/3.14/cabal-package-description-file.html#pkg-section-common-common
[library]:           https://cabal.readthedocs.io/en/3.14/cabal-package-description-file.html#pkg-section-library-library

### Field [cabal-version]

This field goes first in the file and determines the version of the syntax of things to follow.
The [cabal-version] needs to be chosen high enough to support all syntax elements in the file.
We use [cabal-version 2.2] because it is the first to support [common] stanzas.
This version is a good default.

The [cabal-version] should not be higher than the version of [Cabal] shipped with the least version of GHC you want to support.
A table matching GHC to Cabal version can be found at https://www.snoyman.com/base/ or just here:

| GHC  | Date     | base | Cabal | LTS   |
|------|----------|------|-------|-------|
| 8.0  | May 2016 | 4.9  | 1.24  |  9.21 |
| 8.2  | Jul 2017 | 4.10 | 2.0   | 11.22 |
| 8.4  | Mar 2018 | 4.11 | 2.2   | 12.26 |
| 8.6  | Sep 2018 | 4.12 | 2.4   | 14.27 |
| 8.8  | Jul 2019 | 4.13 | 3.0   | 16.31 |
| 8.10 | Mar 2020 | 4.14 | 3.2   | 18.28 |
| 9.0  | Feb 2021 | 4.15 | 3.4   | 19.33 |
| 9.2  | Oct 2021 | 4.16 | 3.6   | 20.26 |
| 9.4  | Aug 2022 | 4.17 | 3.8   | 21.25 |
| 9.6  | Mar 2023 | 4.18 | 3.10  | 22.43 |
| 9.8  | Oct 2023 | 4.19 | 3.10  | 23    |
| 9.10 | Apr 2024 | 4.20 | 3.12  |       |
| 9.12 | Dez 2024 | 4.21 | 3.14  |       |

Note that not each release of [Cabal] necessarily comes with a new [cabal-version], i.e., a `.cabal` file format version.
In particular, there is no [cabal-version] 3.2 nor 3.10.

### Field [name]

The name of the package, same as the name of the `.cabal` file.

### Field [version]

The version of the package, which is a list of natural numbers separated by dots.
There is no limit on the number of positions, but only the first 4 are given a meaning in the Haskell Package Versioning Policy, [PVP].
Versions are lexicographically ordered, so `0 < 0.0 < 0.0.0 < 0.0.0.0 < ... < 0.1 < 0.1.0 < ... < 0.1.1` etc.
It is sometimes surprising that `0` is not the same as `0.0`, because we are used to think in terms of decimal numbers.  To prevent confusions, it is best practice to either
1. fix version numbers to 3 or 4 positions, or
2. not use trailing `0`s.

We opt for the first alternative here, choosing `0.0.0.0`.

The [PVP] and correct versioning will be discussed in section [Dependencies and versioning](#dependencies-and-versioning).

[PVP]: https://pvp.haskell.org/

### Stanza [common]

We define here a [common] stanza `common-build-parameters` that we _import_ in the following [library] stanza.
Importing means basically copying the fields of the [common] stanza.
Currently we import the `common-build-parameters` only once, so its rather pointless to put them into a [common] stanza.  However, we aim to reuse the same parameters in further package components later.

### Stanza [library]

The [library] stanza defines the default component of our package `binary-search-trees`: a library exporting the modules `Data.BST` and shipping the hidden module `Data.BST.Internal`.
These modules are located in subdirectory `src`, as we specify in field [hs-source-dirs].  We could put several source directories there and divide our library code up into these source directories, but this is seldom necessary.
It is best practice to _not_ place the source files directly at the project root.

Any modules we wish to export by the library must be listed explicitly under [exposed-modules].  There is no automatic population of this field from the contents of the [hs-source-dirs].  (Tools like [hpack] do this, though.)
Modules needed for compilation but not exported must be listed under [other-modules].
We placed `Data.BST.Internal` there to hide the implementation.

Any library our code depends on must be listed under [build-depends].  We currently only depend on the Haskell standard library `base` which is shipped with GHC.  Nevertheless, it must be mentioned.
We did not state yet which _versions_ of `base` would work with our code.  Let us put this off until section [Dependencies and versioning](#dependencies-and-versioning).

Finally, we need to specify the version of the Haskell language itself we are using in our code.  These are the fields in `common-build-parameters`:

1. [default-language] sets the base version of Haskell defined by one of the standards `Haskell98`, `Haskell2010`, `GHC2021` or `GHC2024`.  Our choice of `Haskell2010` is a good default (not too old and not to new).

2. We can modify the Haskell language by turning on some [LANGUAGE] extensions for our project in [default-extensions].  Our code needs `LambdaCase` and `InstanceSigs`.

3. Further modification to compilation can be made by passing [ghc-options].
   We turn on the common warning sets [-Wall] and [-Wcompat] to ensure good code quality and forward compatibility.

[hs-source-dirs]: https://cabal.readthedocs.io/en/3.14/cabal-package-description-file.html#pkg-field-hs-source-dirs
[exposed-modules]: https://cabal.readthedocs.io/en/3.14/cabal-package-description-file.html#pkg-field-library-exposed-modules
[other-modules]: https://cabal.readthedocs.io/en/3.14/cabal-package-description-file.html#pkg-field-library-exposed-modules
[build-depends]: https://cabal.readthedocs.io/en/3.14/cabal-package-description-file.html#pkg-field-build-depends

[default-language]: https://cabal.readthedocs.io/en/3.14/cabal-package-description-file.html#pkg-field-default-language
[Haskell98]: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/control.html#extension-Haskell98
[Haskell2010]: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/control.html#extension-Haskell2010
[GHC2021]: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/control.html#extension-GHC2021
[GHC2024]: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/control.html#extension-GHC2024

[default-extensions]: https://cabal.readthedocs.io/en/3.14/cabal-package-description-file.html#pkg-field-default-extensions
[LANGUAGE]: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts.html#language-extensions
[LambdaCase]: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/lambda_case.html#lambda-case
[InstanceSigs]: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/instances.html#extension-InstanceSigs

[ghc-options]: https://cabal.readthedocs.io/en/3.14/cabal-package-description-file.html#pkg-field-ghc-options
[-Wall]: https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag-Wall
[-Wcompat]: https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag-Wcompat

[hpack]: https://hackage.haskell.org/package/hpack

### Lots of boring details?

It seems there is a lot of administrative fuss going on in [Cabal] packaging.  You haven't seen nothing yet!

One reason for the painstaking detail required in `.cabal` files is to ensure building _independent of the local environment_. Our library might be used by others having other GHCs and libraries installed, and it should build in their environment out-of-the-box.

Another reason is that Haskell itself does not have the concept of a _package_ (only that of a _module_).  Thus, packages have to be defined externally, and [Cabal] provides the standard way to do this.

Finally, Haskell, even when starting out first-in-class with the [Haskell 98] report, has not seen consequent standardization subsequently, with no standard produced after [Haskell 2010].
With [Haskell 2020 stalled], only the [GHC2021] and [GHC2024] language extension collections were published.
[GHC2024] (or a successor) might well be the source of the next standard, but in the meantime, we stick to [Haskell2010] and pick our favorite [LANGUAGE] extensions manually.
(Note that [GHC2021] is only available from GHC 9.2 and [GHC2024] only from GHC 9.10.
[GHC2024] finally includes my [favorite extension][survey-extensions] [LambdaCase].)

[Haskell 98]: https://www.haskell.org/onlinereport/
[Haskell 2010]: https://www.haskell.org/onlinereport/haskell2010/
[Haskell 2020 stalled]: https://reasonablypolymorphic.com/blog/haskell202x/
[survey-extensions]: https://taylor.fausak.me/2022/11/18/haskell-survey-results/#s2q5


Interactive development with `cabal repl`
-----------------------------------------

```haskell
$ cabal repl
...
GHCi, version ...
[1 of 2] Compiling Data.BST.Internal ( src/Data/BST/Internal.hs, interpreted )
[2 of 2] Compiling Data.BST          ( src/Data/BST.hs, interpreted )
Ok, two modules loaded.

ghci> :m + Data.BST

ghci> :r
[1 of 2] Compiling Data.BST.Internal ( src/Data/BST/Internal.hs, interpreted ) [Source file changed]
[2 of 2] Compiling Data.BST         ( src/Data/BST.hs, interpreted ) [Source file changed]
Ok, two modules loaded.

ghci> let t :: BST Int = fromList [5,2,4,3]

<interactive>:4:5: error:
    Illegal type signature: ‘BST Int’
      Type signatures are only allowed in patterns with ScopedTypeVariables

ghci> :set -XScopedTypeVariables

ghci> let t :: BST Int = fromList [5,2,4,3]

ghci> t
Node (Node Leaf 2 Leaf) 3 (Node Leaf 4 (Node Leaf 5 Leaf))

ghci> toList t

<interactive>:8:1: error: [GHC-88464]
    Variable not in scope: toList :: BST Int -> t

ghci> :m + Data.Foldable

ghci> toList t
[2,3,4,5]
```

Testsuites
==========

A Cabal package can and should contain test suites that run with `cabal test`.

There are several frameworks to organize and run tests.
The most popular Haskell test framework is [hspec].
We'll use package [tasty] in the following.

[hspec]: https://hackage.haskell.org/package/hspec
[tasty]: https://hackage.haskell.org/package/tasty
[tasty-hunit]: https://hackage.haskell.org/package/tasty-hunit

Unit tests
----------

These are handwritten tests to e.g. check the output of a function for a given input.

The package [tasty-hunit] provides an API for constructing unit tests.

E.g. `UnitTests.hs`:
```haskell
import Data.BST.Internal

import Data.Bifunctor        (bimap)
import Data.Foldable         (toList)

import Test.Tasty            (TestTree, defaultMain)
import Test.Tasty.HUnit      (testCase, assertEqual)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testCase "split" $
    assertEqual prefix actual expect
  where
    prefix = ""
    actual = bimap toList toList $
             split "dog" $
             fromList ["cat", "dog", "tiger", "wolf"]
    expect = (["cat"], ["tiger", "wolf"])
```
We use these API functions:
```haskell
assertEqual :: String -> a -> a -> Assertion
testCase    :: Name -> Assertion -> TestTree
defaultMain :: TestTree -> IO ()
```

We would like to add this testsuite to our project so that `cabal test` runs it.

The problem is that the module `Data.BST.Internal` is not exported by our library,
so we have to find ways how to import it in our test suite.
We discuss a few alternatives in the following.


### Only test the exported modules

Code in branch [no-internal-library].

[no-internal-library]: https://github.com/teach-afp/binary-search-trees/tree/no-internal-library

The unit test we wrote just uses exported functions: `fromList`, `split`, `toList`.
Thus, we could import `Data.BST` instead and depend just on the `library` in our `test-suite`:
```cabal
test-suite unittests
  import: common-build-parameters

  type: exitcode-stdio-1.0

  hs-source-dirs:
    test

  main-is:
    UnitTests.hs

  build-depends:
    -- the library:
    , binary-search-tree
    -- inherited bounds
    , base
    -- new dependencies, need bounds
    , tasty               >= 1.1.0.4 && < 1.6
    , tasty-hunit         >= 0.10    && < 0.12
```
This simple approach will sometimes work.
It will not work when we need constructors of `BST`, as in the [QuickCheck] tests below.

### Test suite includes internal modules as source

`UnitTests.hs` can be added as cabal test-suite to our `.cabal` package file:
```cabal
test-suite unittests
  import: common-build-parameters

  type: exitcode-stdio-1.0

  hs-source-dirs:
    test
    src

  main-is:
    UnitTests.hs

  other-modules:
    Data.BST.Internal

  build-depends:
    -- inherited bounds
    , base
    -- new dependencies, need bounds
    , tasty               >= 1.1.0.4 && < 1.6
    , tasty-hunit         >= 0.10    && < 0.12
```

Now the module `Data.BST.Internal` is shared between the `library` and the `test-suite`.

A drawback is that such shared modules are compiled twice: once for the library and once for the test suite.

### Put internal modules into a internal library

We can move `Data.BST.Internal` our of the main library into an internal library `bst-internal`,
which gets imported by the test suite.
```cabal
-- Internal library containing the implementation

library bst-internal
  import: common-build-parameters

  hs-source-dirs:
    src

  exposed-modules:
    Data.BST.Internal

  build-depends:
    , base                >= 4.12    && < 5

-- Library exporting the API

library
  import: common-build-parameters

  hs-source-dirs:
    lib

  exposed-modules:
    Data.BST

  build-depends:
    , base
    , bst-internal

-- Testsuites

test-suite unittests
  import: common-build-parameters

  type: exitcode-stdio-1.0

  hs-source-dirs:
    test

  main-is:
    UnitTests.hs

  build-depends:
    -- inherited bounds
    , base
    , bst-internal
    -- new dependencies, need bounds
    , tasty               >= 1.1.0.4 && < 1.6
    , tasty-hunit         >= 0.10    && < 0.11
```
Now each module is only compiled once.

Drawback: internal modules still have occasional hiccups in the tooling ([Cabal] and [Stack]).
While `stack repl` works here ([but not always](https://github.com/commercialhaskell/stack/issues/4148)),
`cabal repl` complains that GHCi cannot load two libraries.
We need to invoke it via `cabal repl bst-internal`.

[Stack]: https://www.haskellstack.org/


Property tests
--------------

Using [QuickCheck] we can random-test some laws of our binary search tree operations.

In the following, we build up `QuickCheckTests.hs` step by step.

[QuickCheck]: https://hackage.haskell.org/package/QuickCheck


### Generate random data

To tests properties involving a certain data structure (like `BST`) we need to provide a means to generate elements of this data structure.
Concretely, we need to make it an instance of the QuickCheck class [Arbitrary].

[Arbitary]: https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck-Arbitrary.html#t:Arbitrary

Usually, the `arbitrary` method is implemented via the DSL QuickCheck provides.
For `BST a`, we could first randomly choose a constructor `Leaf` or `Node`.
In the latter case we would call `arbitary` on `a` and recursively on `BST a` to generate the field values of `Node`.
There are several caveats:

* If we choose `Leaf` and `Node` with equal probability, we get lots of boring small trees.
  Half of our trees would be empty.

  **Quiz:** What would be the probability to get a tree of depth at least 2,
  if by depth 0 we mean the empty tree?

* If we set the probability of `Node` too high, `arbitrary` might often not terminate, because it continues to choose `Node` in some branch and never finishes the tree.

  **Quiz:** if the probability of choosing `Node` is _p_ (and `Leaf` _1-p_), what is the probability of termination?

* The approach of randomly choosing constructor and filling the fields does not even make sense for `BST`, since we want trees that are correctly ordered.
  We could switch to randomly choosing `empty` and `insert` instead.

Turns out, though, that we can simply use our method `fromList` to generate well-formed trees from lists.
```haskell
import Test.QuickCheck

instance (Arbitrary a, Ord a) => Arbitrary (BST a) where
  arbitrary = fromList <$> arbitrary
```
This relies on the [Arbitrary] instance for lists, which is shipped with [QuickCheck].

### Properties

The simplest [QuickCheck] properties are relations of any arity.

We formulate some simple properties about membership after insertion or deletion.
```haskell
prop_member_insert x s = member x (insert x s)
prop_member_delete x s = not $ member x (delete x s)
```
These properties relate an element to a tree:
```haskell
prop_member_insert :: (Arbitrary a, Ord a) => a -> BST a -> Bool
prop_member_delete :: (Arbitrary a, Ord a) => a -> BST a -> Bool
```
Often, we do not write the type of the property, in particular, when it is pretty obvious from the property (`member x (insert x s)`) and does not contribute to understanding.

### Equational laws

Many properties are equations, especially those with a "mathematical" flavor (e.g. associativity and unit laws).

We have to be careful to use the right notion of equality.
For `BST`, the equality provided by `Eq` (`==`) is too fine-grained, as it will distinguish trees with different structure even if they have the same elements.

In our case, we get the correct equality by removing the internal structure of the tree and just considering the (ordered) list of its elements.
```haskell
(~~) :: Ord a => BST a -> BST a -> Bool
(~~) = (==) `on` toList
```

This allows us to write some correct equational laws, e.g., that the union of trees makes an idempotent commutative monoid:
```haskell
prop_union_idem  s        = s `union` s ~~ s
prop_union_comm  s1 s2    = s1 `union` s2 ~~ s2 `union` s1
prop_union_assoc s1 s2 s3 = s1 `union` (s2 `union` s3) ~~ (s1 `union` s2) `union` s3

prop_union_unit_left  s   = empty `union` s ~~ s
prop_union_unit_right s   = s `union` empty ~~ s
```

### Constructing a testsuite

It remains to collect and organize the tests into a testsuite.

The [tasty-quickcheck] package provides some convenience to build a `TestTree` from properties.

[tasty-quickcheck]: https://hackage.haskell.org/package/tasty-quickcheck

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Test.Tasty            (TestTree, defaultMain)
import Test.Tasty.QuickCheck (testProperties)

prop_...  -- Our properties start with the prefix 'prop_'.

-- Placing a Template Haskell instruction here forces GHC to finish checking the code above.
-- This makes the splice $allProperties work.
-- The instruction 'return []' inserts an empty list of declarations, thus, does nothing.
return []

tests :: TestTree
tests = testProperties "Tests" $allProperties

main :: IO ()
main = defaultMain tests
```

Via [TemplateHaskell], QuickCheck's [allProperties] collects all functions starting with `prop_`, which we can pass to `testProperties` to get a `TestTree`.
```haskell
allProperties  :: Q Exp
testProperties :: TestName -> [(String, Property)] -> TestTree
```
NB: `Q` is the _quotation monad_ from Template Haskell.

[TemplateHaskell]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/template_haskell.html
[allProperties]: https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck-All.html#v:allProperties

We add `QuickCheckTests.hs` as a cabal testsuite:
```cabal
test-suite quickcheck
  import: common-build-parameters

  type: exitcode-stdio-1.0

  hs-source-dirs:
    test

  main-is:
    QuickCheckTests.hs

  build-depends:
    -- inherited bounds
    , base
    , bst-internal
    -- new dependencies, need bounds
    , QuickCheck          >= 2.11.3  && < 2.16
    , tasty               >= 1.1.0.4 && < 1.6
    , tasty-quickcheck    >= 0.10    && < 0.12

  ghc-options:
    -Wno-orphans
    -Wno-missing-signatures
```
The last field turns off warnings for the orphan instance `Arbitrary (BST a)`
and the missing signatures for the `prop_...` definitions.
(We could also place these options in the file using the `{-# OPTIONS_GHC ... #-}` pragma.)


### Shrinking counterexamples

If we implement the method `shrink :: a -> [a]` in our `instance Arbitrary`,
[QuickCheck] will try to reduce the counterexamples it found against properties:

- If `x` is a counterexample, try if any of `shrink x` is a counterexample as well.
- If yes, continue like this recursively.

An implementation of `shrink` should, as a rule of thumb, produce the _predecessors_ of the given value.
For natural numbers, this would be `pred`.
For lists, that would be lists with one element removed.
In our case, given a `Node l a r`, we either discard the node and return one of the subtrees, or we recursively shrink `l` or `r` or the node label `a`.
```haskell
instance (Arbitrary a, Ord a) => Arbitrary (BST a) where
  arbitrary = ...

  shrink :: BST a -> [BST a]
  shrink = \case
    Leaf -> []
    Node l a r -> concat
      [ [ Leaf ] -- The empty tree (optional, aggressive shrinking).
      , [ l, r ] -- The immediate subtrees.
      , [ Node l' a' r' | (l', a', r') <- shrink (l, a, r) ]
                 -- Shrinking in the subtrees.
      ]
```
(Adapted from the documentation of [shrink].)

[shrink]: https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck-Arbitrary.html#v:shrink


### Further stuff

The [CoArbitrary] instance would allow us to generate random functions:
```haskell
instance CoArbitrary a => CoArbitrary (BST a) where
  coarbitrary = coarbitrary . toList
```

[CoArbitary]: https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck-Arbitrary.html#t:CoArbitrary

Doctests
--------

Unit tests can be included in haddock comments for documentation:
```haskell
-- | Insert into binary search tree.
--
-- Example:
--
-- >>> toList $ insert "baz" $ fromList ["bar","foo"]
-- ["bar","baz","foo"]
--
```
Such a unit test needs to be separated from the surrounding documentation by empty lines.
After the prompt `>>>` we supply the Haskell expression,
the following lines contain the `Show`ed result.

Humans learn easily from examples, thus, this is good documentation to start with.

The [doctest] tool automatically extracts such unit tests and runs them,
comparing the obtained result to the expected result.
Benefits of checking the examples in the documentation:
- Provide unit tests for the package.
- Prevent documentation from rotting.
- Adding doctests is less boilerplate than using e.g. [tasty-hunit].

[doctest]: https://hackage.haskell.org/package/doctest

There are several ways how to run the doctests.

### 1. Doctests as cabal test-suite

Adding a doctest suite to the `.cabal` file:
```cabal
test-suite doctest
  import: common-build-parameters

  type: exitcode-stdio-1.0

  hs-source-dirs:
    test

  main-is:
    DocTests.hs

  build-depends:
    -- inherited bounds
    , base
    -- new dependencies, need bounds
    , doctest >= 0.22 && < 0.25
```
We need a stub `DocTests.hs` that runs the `doctest` function on the files that contain tests:
```haskell
import Test.DocTest    ( doctest )

main :: IO ()
main = doctest $ concat
  [ map ("-X" ++)
    -- All language extensions needed by the tests
    [ "LambdaCase"
    , "InstanceSigs"
    ]
    -- Files containing doctests
  , [ "src/Data/BST/Internal.hs"
    ]
  ]
```
The `doctest` library uses the `ghc` library to extract tests and expected results from the haddock comments.

Unfortunately, the information in the `.cabal` file is not used automatically.
We have to pass the language extensions manually, as well as the list of source files.
Also, the source files may need `$setup` code e.g. to make imports available to the doctests.

Evaluation of calling the `doctest` library function:
- Drawback: violates DRY ("don't repeat yourself"), brittle.
- Advantage: runs automatically with `cabal test`.

### 2. Doctests via the `doctest` executable

We can also run the doctests via the `doctest` executable.
This needs an installation of the `doctest` executable _matching exactly the GHC version_ (see `ghc -V`).
To use `doctest` with GHC _x.y.z_, `doctest` needs to be compiled with GHC _x.y.z_.
Basically, `doctest` is a modified `ghci`.

Invocation (from project root).
```
cabal build
cabal repl --with-compiler doctest
```
Short:
```
cabal repl -w doctest
```
It often makes sense to turn off warnings:
```
cabal repl -w doctest --repl-options=-w
```

Evaluation of calling the `doctest` executable:
- Drawback: needs installation of `doctest`.
- Advantage: uses information from `.cabal` file, more stable.

### 3. Other doctest solutions

- [cabal-doctest]: provide information from `.cabal` file for the `doctest` function call.
  Drawback: needs `build-type: custom` and custom `Setup.hs`.

- [doctest-parallel]: Fork of [doctest] to run doctests in parallel.

[cabal-doctest]: https://hackage.haskell.org/package/cabal-doctest
[doctest-parallel]: https://hackage.haskell.org/package/doctest-parallel


Dependencies and versioning
===========================

Each dependency of our package should come with compatibility bounds.
```cabal
  build-depends:
    , base                >= 4.12    && < 5
    , QuickCheck          >= 2.11.3  && < 2.16
    , tasty               >= 1.1.0.4 && < 1.6
    , tasty-quickcheck    >= 0.10    && < 0.12
```

The Haskell Package Versioning Policy ([PVP]) specifies 4 version digits/positions:
_edition.major.minor.patch_.
E.g. `base` for GHC-9.8.4 has version `4.19.2.0`.

Each Hackage release needs a new version number.
The [PVP] specifies what kind of version bump is required.

|  position  | example | bump when ...          |
|------------|---------|------------------------|
| _edition_  |    4    | (big) API change       |
| _major_    |   19    | API change             |
| _minor_    |    2    | API additions only     |
| _patch_    |    0    | bug and doc fixes only |

The _edition_ (my terminology) is not present in SemVer and is rather freely chosen.
Radical API changes should maybe bump the _edition_ (e.g. `aeson-1 → aeson-2`),
but it is up to the maintainer to decide.
The pair _edition.major_ makes the major version of a package, the triple
_edition.major.minor_ the minor version.

Contracts:
- Your package will continue to build if its dependencies only bump _patch_.
- If your package only uses qualified imports,
  it will even continue to build when its dependencies only bump _minor_.

Determine upper bounds:
- Use only qualified imports.
- If your package builds with the latest release _x.y.z.u_ of a dependency,
  set the upper bound to _< x.(y+1)_.

Determine lower bounds:
- Check when the functions you need from a dependency entered its API.
  E.g. all functions were present at version _x.y.z.u_,
  set the lower bound to _>= x.y.z.u_ or _>= x.y.z_.
- Optionally raise the lower bound if your other deps need a newer version of this dep.

Special dependency `base`:
- Each major GHC version includes a new major version of `base`.
- `base` serves as a proxy for the GHC version.
- Actual changes in `base` are usually minor.

Problems of upper bounds
------------------------

We cannot see the future.
We cannot _know_ now whether a future _major_ bump of a dependency will break our build.
While the meaning of lower bounds is unambiguous, we can interpret upper bounds in two ways:

1. The "pessimistic" interpretation of an upper bound:
   _dep < version_ means that we have __no evidence__ that building with _dep ≥ version_ will __succeed__.

2. The "optimistic" interpretation of an upper bound:
   _dep < version_ means that we have __evidence__ that building with _dep ≥ version_ __fails__.

The pessimistic interpretation guarantees the PVP, but may require lots of upward revisions of upper bounds.

The optimistic interpretation might lead to broken builds which need downward revisions of upper bounds.

Pragmatically, some maintainers do not specify upper bounds or just _edition_ level ones like `base < 5`.
This may require quick intervention to repair build failures when new versions of dependencies are released.

Stack solves the problem of dependency compatibility by defining Hackage _snapshots_.

Stackage
--------

[Stackage] defines _snapshots_ of its package collection.
A snapshot is named e.g. [lts-21.25] or [nightly-2025-03-01]
and contains a single version of each package in the collection.

[Stackage]: https://www.stackage.org/
[lts-21.25]: https://www.stackage.org/lts-21.25
[nightly-2025-03-01]: https://www.stackage.org/nightly-2025-03-01

A `nightly` snapshot usually contains the latest versions from Hackage with exceptions (e.g. if a dependencies is so new that packages using it have not caught up to it).
The original idea was that `nightly` tracks the latest GHC, but nowadays it is behind by one major version.

A `lts` (Long Term Service) snapshot tracks usually an older, established version of GHC.

A snapshot is specified in a `stack.yaml` file (only mandatory entry):
```yaml
resolver: lts-21.25
```

A `stack.yaml` file can also add or replace dependencies in a snapshot, using `extra-deps`:
```yaml
extra-deps:
- my-cool-pkg-0.2.0.1
```

[Stack] serves as an alternative to [Cabal] and provides a similar command set, e.g.
```bash
stack build
stack install
stack test
stack repl
```

Continuous integration
======================

Continuous integration (CI) can be utilized to regularly build and test a project in various environments
(e.g. different OSs, different GHC version, different stackage snapshots).

E.g. https://github.com donates free CI to open source developments via [GitHub Actions].

[GitHub Actions]: https://docs.github.com/en/actions

### Haskell-CI wizard

[Haskell CI] autogenerates a multi-GHC CI script for Ubuntu machines from the `tested-with` field in the `.cabal` file.

[Haskell CI]: https://github.com/haskell-CI/haskell-ci

```cabal
tested-with:
  GHC == 9.10.1
  GHC == 9.8.4
  GHC == 9.6.6
  GHC == 9.4.8
  GHC == 9.2.8
  GHC == 9.0.2
  GHC == 8.10.7
  GHC == 8.8.4
  GHC == 8.6.5
```

Configuration can be given on the command line or in `cabal.haskell-ci`.

```bash
$ haskell-ci github binary-search-tree.cabal
*INFO* Generating GitHub config for testing for GHC versions:
8.4.4 8.6.5 8.8.4 8.10.7 9.0.2 9.2.8 9.4.8 9.6.6 9.8.4 9.10.1
```
This generates `.github/workflows/haskell-ci.yml`.

Note:
- Haskell CI is seldom released to Hackage.
  I recommend to install the latest version from the [Haskell CI] github repository.
- Haskell CI unfortunately not very well-documented.


### Writing a simple Stack CI

A simple CI using stack could look like this `.github/workflows/simple-stack.yml`:
```yaml
name: Simple Stack CI

jobs:
  build:
    name: Stack Simple CI
    runs-on: ubuntu-latest

    steps:

    - name: Checkout sources from repository
      uses: actions/checkout@v4

    - name: Setup haskell for use with stack
      uses: haskell-actions/setup@v2
      with:
        ghc-version: '9.4.8'
        enable-stack: true
        cabal-update: false

    - name: Build
      run:  stack --system-ghc --stack-yaml=stack-9.4.8.yaml build

    - name: Test
      run:  stack --system-ghc --stack-yaml=stack-9.4.8.yaml test

    - name: Build docs
      run:  stack --system-ghc --stack-yaml=stack-9.4.8.yaml haddock
```
This defines a single job with identifier `build` that runs on Ubuntu Linux and performs the following steps:
1. Check out the default branch of the repository.
2. Install GHC 9.4.8 and Stack via the https://github.com/haskell-actions/setup action.
3. Run `stack build` using the installed GHC and the resolver as specified in `stack-9.4.8.yaml`.
4. Run `stack test`.
5. Build the documentation via `stack haddock`.

Before pushing the script, it makes sense to check for errors using [`actionlint`](https://github.com/rhysd/actionlint).

We might want test CI on several GHCs.  To this end, we can use build matrices:

### A CI using a build matrix

We extend the previous CI to run on different OSs and different GHCs.
To this end, we utilize the `matrix` `strategy` feature:
```yaml
jobs:
  build:
    name: Stack ${{ matrix.ghc }} ${{ matrix.os }}
    runs-on: ${{ matrix.os }}
    strategy:
      fail-fast: false
      matrix:
        os: [ubuntu-latest]
        ghc: ['9.10.1', '9.8.4', '9.6.6', '9.4.8', '8.6.5']
        include:
        - os: windows-latest
          ghc: '9.10.1'
        - os: macos-latest
          ghc: '9.10.1'

    steps:

    - name: Checkout sources from repository
      uses: actions/checkout@v4

    - name: Setup haskell for use with stack
      uses: haskell-actions/setup@v2
      with:
        ghc-version: ${{ matrix.ghc }}
        enable-stack: true
        cabal-update: false

    - name: Build
      run:  stack --system-ghc --stack-yaml=stack-${{ matrix.ghc }}.yaml build

    - name: Test
      run:  stack --system-ghc --stack-yaml=stack-${{ matrix.ghc }}.yaml test

    - name: Build docs
      run:  stack --system-ghc --stack-yaml=stack-${{ matrix.ghc }}.yaml haddock
```
This will run 7 copies of the `build` job, 5 on `ubuntu-latest` and one each on `windows-latest` and `macos-latest`.
Setting `fail-fast: false` will run each job to completion even if some of them fail.
The variables are referred to by `matrix.os` and `matrix.ghc` and their value can be used inside `${{ ... }}` escapes.

In larger projects it might make sense to cache the build products, especially when building takes long.

### CI with caching

We add caching of the global and local stack working directories.
To this end, we give the setup step an identifier via `id: setup`.
The setup action has outputs `stack-version` and `ghc-version` that we can use as parts of the cache key.
The other components are the OS (`runner.os`) and the hash of the commit we are testing (`github.sha`).

After setting up Haskell, we attempt to restore the cache from a previous CI run using the `actions/cache/restore` action.
Of course, since we include the commit hash in the cache key, a cache entry cannot be found, and we fall back to one of our `restore-keys`.
In fact, we have just one backup key that is composed of OS, Stack and GHC version.
We restore the global stack directory as provided by the setup action as `stack-root` output,
and the local `.stack-work` directory.

After building, we cache these directories under the full key which we copy from the restore step via the output `cache-primary-key` (we could also have repeated the key construction, but DRY).
The special condition `always()` makes sure the step executes even if one of the previous steps failed.
This way, we can salvage partial work in building the project.

```yaml

    steps:

    - name: Checkout sources from repository
      uses: actions/checkout@v4

    - name: Setup haskell for use with stack
      id:   setup
      uses: haskell-actions/setup@v2
      with:
        ghc-version: ${{ matrix.ghc }}
        enable-stack: true
        cabal-update: false

    - name: Restore cache
      id:   cache
      uses: actions/cache/restore@v4
      env:
        key: ${{ runner.os }}-stack-${{ steps.setup.outputs.stack-version }}-ghc-${{ steps.setup.outputs.ghc-version }}
      with:
        key: ${{ env.key }}-hash-${{ github.sha }}
        restore-keys: ${{ env.key }}
        path: |
          ${{ steps.setup.outputs.stack-root }}
          .stack-work

    - name: Build
      run:  stack --system-ghc --stack-yaml=stack-${{ matrix.ghc }}.yaml build

    - name: Test
      run:  stack --system-ghc --stack-yaml=stack-${{ matrix.ghc }}.yaml test

    - name: Build docs
      run:  stack --system-ghc --stack-yaml=stack-${{ matrix.ghc }}.yaml haddock

    - name: Save cache
      if:   always()
        ## Save cache regardless of whether on of the steps failed
      uses: actions/cache/save@v4
      with:
        key: ${{ steps.cache.outputs.cache-primary-key }}
        path: |
          ${{ steps.setup.outputs.stack-root }}
          .stack-work
```

Github workflows 101:

- Each job may run on a different machine.
  To communicate result from one job to the next, define outputs, or upload artifacts!

- Each step runs in a new process instance.
  In particular, changes to the environment are not preserved.
  Results can be communicated from one step to later ones:
  * By outputs (writing to `${GITHUB_OUTPUT}`).
  * Via the file system.
  * Writing to the special files `${GITHUB_ENV}` (to add environment settings)
    and `${GITHUB_PATH}` (to add to the `PATH`).

- Some Haskell libraries bind to system libraries you might need to install first.


Resilient and maintainable code
===============================

Software evolves.  What style of coding leads to maintainable code?

Modelling data
--------------

Make custom and tight-fitting data structures!

### Prefer `newtype` over type synonyms

```haskell
-- | Reversed list
type SnocList a = [a]

reverseAppend :: [a] -> SoncList a -> SoncList a
reverseAppend (x:xs) ys = reverseAppend xs (x:ys)
...
```
Problems:

- We might accidentially supply a list instead of a reverse list,
  the type checker will not notice.
- Type synonym gets unfolded, thus,
  type errors might speak be about lists `[...]` instead of `SnocList ...`.

```haskell
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | Reversed list
newtype SnocList a = SnocList { theSnocList :: [a] }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

reverseAppend :: [a] -> SoncList a -> SoncList a
reverseAppend xs (SnocList ys) = SnocList (revApp xs ys)
  where
    revApp = ...
```

- Safe: no mixup between `[a]` and `SnocList a`.

- No performance penalty, at runtime `SnocList a` is just `[a]`,
  the wrappers
  ```haskell
  SnocList :: [a] -> SnocList a
  theSnocList :: SnocList a -> [a]
  ```
  are identity functions and optimised away.

- Cost: needs some boilerplate code.

  Real-world example from the Agda code base:
  [commit refactoring a `type` into `newtype`](https://github.com/agda/agda/commit/0f95e339a6c9ecfc96c984960177886627421d19)

### Meaningful numeric types

Give meaning to numeric types (example: turtle graphics):

- Bad: direct use of numeric type.
  ```haskell
  type Program

  right   :: Double -> Program
  forward :: Double -> Program
  ```

- Better: type synonyms.
  ```haskell
  type Angle    = Double
  type Distance = Double

  right   :: Angle    -> Program
  forward :: Distance -> Program
  ```

- Diligent: `newtype`.
  ```haskell
  newtype Angle = Angle { angleToDouble :: Double }
    deriving (Num, ...)
  newtype Distance = Distance { distanceToDouble :: Double }
    deriving (Num, ...)
  ```
  But: Haskell's numeric classes are not the pinnacle of design.

  Angle and distance are (1-dimensional) vector spaces:
  we have addition and scalar multiplication, but not general multiplication:
  ```haskell
  (*) :: Num a => a -> a -> a
    -- makes no sense for 'Angle' and 'Distance'
  ```

- Expert: account for different angle units.
  ```haskell
  type Angle
  degreeAngle :: Double -> Angle
  radianAngle :: Double -> Angle
  ```

### Meaningful booleans

Make your own copies of `Bool`.

- Bad: direct use of `Bool`.
  ```haskell
  data TurtleState = TurtleState {
    ...
    , penState :: Bool -- ^ 'True' means drawing, 'False' not drawing.
    ...
    }

  setPen :: Bool -> TurtleState -> TurtleState
  ```

- Better: type synonym.
  ```haskell
  data TurtleState = TurtleState {
    ...
    , penState :: PenState
    ...
    }

  -- | 'True' means drawing, 'False' not drawing.
  type PenState = Bool

  setPen :: PenState -> TurtleState -> TurtleState
  ```

- Recommended: custom `data` type.
  ```haskell
  data PenState
    = PenUp   -- ^ Not drawing.
    | PenDown -- ^ Drawing.
  ```

- Alternative: `newtype PenState = PenState Bool`.

Advantages:
- code is easier to comprehend
- code is searchable (e.g. for `PenUp`; try searching for `False`)


### Custom `data` rather than `Maybe` / `Either` / tuples

Unreasonable:
```haskell
completePersonData :: Either String (String, String) -> (String, String, String)
```
Reasonable: custom data structures, use `Text`.
```haskell
import Data.Text (Text)

newtype PersonNumber = PersonNumber Text
data Name = Name
  { firstName :: Text
  , lastName  :: Text
  }
data PersonData = PersonData
  { personName   :: Name
  , personNumber :: PersonNumber
  }

completePersonData :: Either PersonNumber Name -> PersonData
```
`Either` is fine here.

But:
```haskell
completePersonData :: Either PersonNumber (Either Email Name) -> PersonData
```
Not fine! Rather:
```haskell
data PersonIdentification
  = ByPersonNumber PersonNumber
  | ByEmail        Email
  | ByName         Name
```


### Pattern matching instead of projections

As Haskell software evolves, new constructors may be added to `data` types
or new fields to constructors.

Using the coverage checker `{-# OPTIONS_GHC -Wincomplete-patterns #-}`
helps detecting functions that need to be extended to handle the new cases or situation.

However, this works only if we implement functions in a certain style:
- Use pattern matching instead of selectors.
- Avoid catch-alls, spell out all cases.
- Spell out all constructor arguments.

Example:
```haskell
data BST a
  = Leaf
  | Node (BST a) a (BST a)

isNode :: BST a -> Maybe (BST a, a, BST a)
isNode = \case
  Node l a r -> Just (l,a,r)
  _ -> Nothing

insert :: a -> BST a -> BST a
insert k t
  | Just (l, p, r) <- isNode t =
      ...
  | otherwise =
      Node Leaf k Leaf
```
If we extend `BST` we are not informed that we need to update `insert`:
```haskell
data BST a
  ...
  | LeafNode a
```
`insert` will now lose `LeafNode`s.

Rewriting `insert` to match on each case prevents such bit-rot.


E.g. https://github.com/agda/agda/commit/9702781671c621e638e905fbec2a6a2a1083613e


### Documentation and style

Document:

- Each `data` type.
  * The purpose of the type.
  * Each of its constructors.
  * Each of the constructor fields.

  ```haskell
  -- | Binary search trees: ordered, not necessarily balanced trees.
  data BST a
      -- | Empty search tree.
    = Leaf
      -- | Binary node in search tree.
    | Node
        (BST a)  -- ^ Left subtree, with elements ≤ label.
        a        -- ^ Node label (pivot element).
        (BST a)  -- ^ Right subtree, with element ≥ label.
  ```

- Each function.
  * The purpose of the function.
  * Maybe an example (can be doctested).
  * The role of each argument and preconditions.
  * The result and postconditions.

  ```haskell
  -- | Create a binary search tree from a list.
  fromList :: Ord a
    => [a]   -- ^ List to convert into tree.
    -> BST a -- ^ Tree, not containing duplicates.
  ```

Style:

- Haskell is very expressive.
  One can write very abstract code.
  Don't overdo it!

- Pointfree style.
  Use modestly!
  Excessive point-free style makes code unreadable:
  `flip . ((flip . (flip .)) .)` vs. `\ f a b c d -> f b c d a`.

Some advice:
- https://github.com/andreasabel/haskell-style-guide (not recently updated)
- https://wiki.haskell.org/Pointfree
