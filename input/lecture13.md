# Type-level programming

## Design of DSLs

* In our DSLs, we have tree important components: constructors, combinators, and
  run functions

* Sometimes, the DSL type-signature for combinators might allow to construct
  objects such that have *no semantics*

  <div class = "alert alert-info">
  Graphic of DSL 1
  </div>

* As a result, run functions need to *dynamically* check that objects "make
  sense" before *running* them

## A simple (non-monadic) DSL for integers and booleans expressions

* The DSL

  ```haskell
  -- Type of objects
  data Expr

  -- Constructors
  int  :: Int  -> Expr
  bool :: Bool -> Expr

  -- Combinators
  addc :: Expr -> Expr -> Expr
  eqc  :: Expr -> Expr -> Expr
  ifc  :: Expr -> Expr -> Expr -> Expr
  ```
