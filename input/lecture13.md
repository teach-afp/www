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
  plus  :: Expr -> Expr -> Expr
  equal :: Expr -> Expr -> Expr
  if    :: Expr -> Expr -> Expr -> Expr

  -- Run
  type Value

  eval :: Expr -> Value
  ```

* Observe from the interface that the combinators can compose elements of
  different nature, which might trigger errors in the run function

  What is the *semantics* of the following expressions?

  ```haskell
  ifc (lit 3) (lit 42) (bool False)```

  ```haskell
  eqc (lit 3) (bool True)```

  <div class = "alert alert-info">
  The DSL allows to build both well- and ill-typed expressions!
  </div>

* We can start given a complex (and arguably wrong) semantics to *bad
  expressions* (JavaScript does that!), e.g., `1 + True == 1`, etc.

  <div class = "alert alert-info">
  We assume that ill-typed expressions have no semantics!
  </div>

  Therefore, the run function needs to trigger errors when expressions are bad.

## An EDSL implementation

* We implement `Expr` as follows

  ```haskell
  data Expr where
    LitI   :: Int  -> Expr
    LitB   :: Bool -> Expr
    (:+:)  :: Expr -> Expr -> Expr
    (:==:) :: Expr -> Expr -> Expr
    If     :: Expr -> Expr -> Expr -> Expr ```

* The interesting case is the run function

  ```haskell
  eval :: Expr -> Value
  ```

  Since expressions can be reduced to integers or booleans, an element of type
  `Value` is either an integer or a boolean.

  ```haskell
  data Value = VInt Int | VBool Bool ```

  The run function then checks that expressions are evaluated to *the expected
  type of values* at every combinator.

  ```haskell
  eval :: Expr -> Value
  eval (LitI n)       =  VInt n
  eval (LitB b)       =  VBool b
  eval (e1 :+: e2)     =  plus (eval e1) (eval e2)
    where plus (VInt n) (VInt m) = VInt $ n + m
          plus _        _        = error "plus: type error"
  eval (e1 :==: e2)    =  eq (eval e1) (eval e2)
    where eq (VInt n)  (VInt m)  = VBool $ n == m
          eq (VBool a) (VBool b) = VBool $ a == b
          eq _         _         = error "equal: type error"
  eval (If e1 e2 e3)  =  case eval e1 of
    VBool True   -> eval e2
    VBool False  -> eval e3
    _            -> error "if: type error" ```

* Ill-typed expressions raise errors when evaluating them

  ```haskell
  eBad2 = If (LitI 20) (LitI 1) (LitI 7) ```

  ```haskell
  > eval eBad2
  *** Exception: if: type error! ```

* The picture




* Can we avoid constructing bad expressions at all?

## A EDSL with type-aware combinators

* Now, for every type, we have a new *kind of expressions*

  ```haskell
  data ExprI  -- Integer expressions
  data ExprB  -- Boolean expressions ```

* Combinators can be defined on the right "kind of expressions"

  ```haskell
  (:+:) :: ExprI -> ExprI -> ExprI
  If    :: ExprB -> ExprI -> ExprI -> ExprI ```

  While this looks attractive, and indeed helps, it requires to introduce *many
  new constructors* to deal with all the possible "good" ways to build
  expressions.

  ```haskell
  data ExprI where
    LitI  :: Int -> ExprI
    (:+:) :: ExprI -> ExprI -> ExprI
    IfI   :: ExprB -> ExprI -> ExprI -> ExprI

  data ExprB where
    LitB :: Bool -> ExprB
    EqI  :: ExprI -> ExprI -> ExprB
    EqB  :: ExprB -> ExprB -> ExprB
    IfB  :: ExprB -> ExprB -> ExprB -> ExprB ```

  Observe the *reproduction* of equality constructors (`EqI` and `EqB`) and
  branches (`IfI` and `IfB`) depending on the type of arguments they take.

* There are two run functions, one for each type of expressions

  ```haskell
  evalI :: ExprI -> Int
  evalI (LitI n)    = n
  evalI (e1 :+: e2) = evalI e1  +  evalI e2
  evalI (IfI b t e) = if evalB b then evalI t else evalI e

  evalB :: ExprB -> Bool
  evalB (LitB b)    = b
  evalB (EqI e1 e2) = evalI e1  ==  evalI e2
  evalB (EqB e1 e2) = evalB e1  ==  evalB e2
  evalB (IfB b t e) = if evalB b then evalB t else evalB e ```

  Observe that `evalA` and `evalB` do not raise type-errors.

* We can use this EDSL to validate a value of type `Expr`

* In other words, we take a value of type `Expr` and map it into either `ExprI`
  or `ExprB`.

  If successful, we can run a value of type `Expr` using `evalA` or `evalB`, so
  no type errors!

  <div class = "alert alert-info">
  Graphic!
  Expr -> ExprI  --> evalA --> Value
          ExprB  --> evalB --> Value
  </div>

* We need to write a function which *infers* the type for a value of type
  `Expr`

  ```haskell
  infer :: Expr -> ? ```

  There are three possible outcomes for `infer e`, i.e., it might produce an
  error (`e` has no type) or either a value of type `ExprI` or `ExprB`.

  ```haskell
  import qualified Expr as E

  data TypedExpr = TInt ExprI | TBool ExprB

  infer :: E.Expr -> Maybe TypedExpr ```

  The easy cases are the constructors.

  ```haskell
  infer (E.LitB b)    = return $ TBool (LitB b)
  infer (E.LitI i)    = return $ TInt  (LitI i) ```

  For addition, we need both argument to be integer expressions.

  ```haskell
  infer (e1 E.:+: e2)  = do
   te1 <- infer e1
   te2 <- infer e2
   inferPlus te1 te2 ```

  Function `inferPlus` checks that both `te1 :: TypedExpr` and `te2 ::
  TypedExpr` are integers and, if that is the case, it constructs the
  corresponding value of type `ExprI`, i.e., a sum.

  ```haskell
  inferPlus :: TypedExpr -> TypedExpr -> Maybe TypedExpr
  inferPlus (TInt e1) (TInt e2) = Just (TInt (e1 :+: e2))
  inferPlus _         _         = Nothing ```

  In a similar manner, we write the cases for equality and branches.

  ```haskell
  infer (e1 E.:==: e2) = do
    te1 <- infer e1
    te2 <- infer e2
    inferEq te1 te2
  infer (E.If b t e)  = do
    tb <- infer b
    tt <- infer t
    te <- infer e
    inferIf tb tt te ```

  <div class = "alert alert-info">
  **Exercise**: write `inferEq` and `inferIf`
  </div>

* We write `eval` only for typed expressions so we guarantee no type errors

  ```haskell
  eval :: TypedExpr -> E.Value
  eval (TInt  ei) = E.VInt  $ evalI ei
  eval (TBool eb) = E.VBool $ evalB eb ```

* Problems with this approach:

  - Duplication of constructors to denote "the same operation" but on different
    type of expressions

  - It does not scale up for *type constructors* (e.g., lists, tuples, monads, etc.)

    For instance, if we want to introduce lists, we need to define a type for
    list expressions

    List of integers.
    ```haskell
    data ExprLI where
         EmptyLI  :: ExprLI
         ConsLI   :: ExprI  -> ExprLI -> ExprLI
         ConcatLI :: ExprLI -> ExprLI -> ExprLI ```

    List of booleans.
    ```haskell
    data ExprLB where
         EmptyLB  :: ExprLB
         ConsLB   :: ExprB  -> ExprLB -> ExprLB
         ConcatLB :: ExprLB -> ExprLB -> ExprLB ```

    What about having lists of lists?

    List of lists of integers.
    ```haskell
    data ExprLLI where
         EmptyLLI  :: ExprLLI
         ConsLLI   :: ExprLI  -> ExprLLI -> ExprLLI
         ConcatLLI :: ExprLLI -> ExprLLI -> ExprLLI ```

    List of lists of booleans.
    ```haskell
    data ExprLLB where
         EmptyLLB :: ExprLLB
         ConsLLB  :: ExprLB  -> ExprLLB -> ExprLLB
         Concat   :: ExprLLB -> ExprLLB -> ExprLLB ```

    What about lists of lists of lists of integers/booleans?
