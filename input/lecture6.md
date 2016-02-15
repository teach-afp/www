# Monad transformers


## Motivation

* Programs often perform several side-effects
  - Error reporting
  - Stateful computations
  - I/O

* So, you can imagine a super-monad that does all of that

  <div class="container">
     <img class="img-responsive col-md-6"
       src="./assets/img/supermonad.png">
  </div>

* What happens if I need a monad with less effects?

  - Redefine that definition for `m a`, adapt `return` and `(>>=)` as well as
    the non-proper morphisms.

* What happens if I need a monad with more effects?

  - Redefine that definition for `m a`, adapt `return` and `(>>=)` as well as
    the non-proper morphisms.

* We want to compose effects in a modular manner.

  <div class="container">
     <img class="img-responsive col-md-10"
       src="./assets/img/monad_trans.png">
  </div>

  We would like non-proper morphisms, `return`, and `(>>=)` require minimum
  modifications when effects are added/removed.

* Monad transformers help us to do that

  <div class="container">
     <img class="img-responsive col-md-10"
       src="./assets/img/monad_trans2.png">
  </div>

  <div class="alert alert-info">
   It allows us to combine effects on demand!
  </div>

  <div class="alert alert-danger">
  Not all the possible
  combinations of effects can be modularly combined!
  </div>

  Monad transformers is not the perfect solution, but it certainly helps (we
  will discuss more of the consequences of combining  effects in the next lecture).

## Running example: a simple interpreter

* During this lecture, we consider a simple monadic interpreter.

* The interpreter underlying monad is going to be modularly enhanced with
  different effects.

* A simple (non-monadic) interpreter of expressions

  ```haskell
  data Expr =   Lit Integer
              | Expr :+: Expr

  s_eval :: Expr -> Integer
  s_eval (Lit n)     = n
  s_eval (e1 :+: e2) = s_eval e1 :+: s_eval e2 ```

  This code is not monadic, but we can change that.

## Interpreter0: no side-effects
[code](https://bitbucket.org/russo/afp-code/src/HEAD/L6/Interpreter0.hs?at=master&fileviewer=file-view-default)

* Any code can be "lifted" into the *identity monad*, i.e., a monad with no
  side-effects.

   <div class="container">
      <img class="img-responsive col-md-10 "
        src="./assets/img/monad_id.png">
   </div>

  ```haskell
  import Control.Monad.Identity
  type Eval a = Identity a
  ```

  Type `Eval a` denotes a monadic computation.

* We change the type of the interpreter to return a monadic computation

  ```haskell
   eval :: Expr  -> Eval Integer
   eval (Lit n)    = return n
   eval (a :+: b)  = (+) <$> (eval a) <*> (eval b) ```

   We leverage the function `runIdentity :: Identity a -> a` to run the interpreter.

  ```haskell
   runEval :: Eval a -> a
   runEval = CMI.runIdentity ```

  There are no effects, it just computes the result. More formally, we have that

  ```haskell
   runEval . eval  ≡ s_eval  ```

* We want to extend our interpreter with new features
  - Local declarations
  - Exceptions
  - References

* How are we going to extend the `Eval` monad?

## Enter monad transformers

* Monad transformers are type-level functions

  <div class="container">
     <img class="img-responsive col-md-8"
       src="./assets/img/monadt_func.png">
  </div>

* A certain monad transformer `T` takes a monad `m` and produces a
  monad with additional side-effects

* Every monad transformer `T` maps monads into a set of monads
  with the side-effects added by `T`


## Interpreter1: the reader monad transformer
[code](https://bitbucket.org/russo/afp-code/src/HEAD/L6/Interpreter1.hs?at=master&fileviewer=file-view-default)

* We want to extend our language of expression with *local bindings*, e.g., we
  would like to run a program like `let x = 5; x+x`

* We extend our data type for expressions

  ```haskell
  data Expr = Lit Integer
            | Expr :+: Expr
            | Var Name            -- new
            | Let Name Expr Expr  -- new ```

* Bindings are *immutable*, i.e., once a variable is bound to a value, it cannot
  be changed

* Expression can now involve bound variables, e.g., `x+y`.

* To evaluate expressions, `eval` should include an *immutable environment*
  which contains the values for bound variables.

* We model environments as mappings from variables names to values, i.e., think
  it a mapping as an element of type `[(Name, Value)]` or `Name -> Maybe Value`.

  We use mappings from the module `Data.Map`

  ```haskell
   type Env = Map Name Value

   emptyEnv = Map.empty
   Map.lookup :: Ord k => k -> Map k a -> Maybe a
   Map.insert :: Ord k => k -> a -> Map k a -> Map k a ```

* We would like to implement a monad with a read-only environment since bindings
  are immutable

* One alternative is to hard-wired it into the interpreter

  ```haskell
  eval :: Env -> Expr -> Eval Value ```

  This means that there would be some *plumbing* to pass the environment
  between recursive calls (recall lecture 3).

* Instead, we use the monad transformer `ReaderT` to add a *read-only
  state* into the `Eval` monad!

  <div class="container">
     <img class="img-responsive col-md-8"
       src="./assets/img/monadt_reader.png">
  </div>

  In other words, `ReaderT` takes a monad `m` and returns a monad which support
  two operations: `local` and `ask` -- such monads are called `MonadReader`.

  <div class="alert alert-info">
   A monad obtained by the transformer `ReaderT` is a`MonadReader`, but not all
   `MonadReader` are obtained by using the monad transformer `ReaderT`!
  </div>

   ```haskell
   data ReaderT s m a

   runReaderT :: ReaderT s m a -> (s -> m a)

   local      :: MonadReader s m => (s -> s) -> m a -> m a
   ask        :: MonadReader s m => m s ```

* The monad `Eval` is then responsible for the *plumbing* required to pass around
  the environment.

   ```haskell
   newtype Eval a = MkEval (ReaderT Env (Identity) a)
     deriving (Functor, Applicative, Monad, MonadReader Env) ```

   <div class="container">
      <img class="img-responsive col-md-10 "
        src="./assets/img/monad_r_id.png">
   </div>

   We introduce a new data type, rather than just a type synonym, because we
   want Haskell to derive some type-classes instances.

   Due to deriving `MonadReader Env`, our evaluation monad supports

   ```haskell
   local :: MonadReader Env Identity => (Env -> Env) -> Identity a -> Identity a
   ask   :: MonadReader Env Identity => Identity Env ```

* Let us define some environment manipulation based on `local` and `ask`.

  Looking up the value of a variable in the enviroment.

  ```haskell
   lookupVar :: Name -> Eval Value
   lookupVar x = do
     env <- ask
     case Map.lookup x env of
       Nothing -> fail $ "Variable " ++ x ++ " not found."
       Just v  -> return v ```

    We can extend the environment with a new binding for a local
    computation.  Since we are using a reader monad, we can be sure
    that this binding does not escape outside its intended scope.

    ```haskell
     localScope :: Name -> Value -> Eval a -> Eval a
     localScope n v = local (Map.insert n v) ```

* The interpreter is extended by simply adding cases for the
  two new constructs. (None of the old cases has to be changed.)

  ```haskell
   eval :: Expr -> Eval Value
   eval (Lit n)       = return n
   eval (a :+: b)     = (+) <$> eval a <*> eval b
   eval (Var n)       = lookupVar n                 -- here, we use ask
   eval (Let n e1 e2) = do v <- eval e1
                           localScope n v (eval e2) -- here, we use local ```

* The `run` function is simply run the interpreter with the initial environment
  empty.

   ```haskell
   runEval :: Eval a -> a
   runEval (MkEval reader) = runIdentity (runReaderT reader emptyEnv) ```

   Observe how we run the reader monad first, and then the identify monad.

   When using monad transformers, you start running the outermost monad and
   finish with the innermost one of your *monad stack*.

   <div class="container">
      <img class="img-responsive col-md-11 "
        src="./assets/img/monad_r_id_run.png">
   </div>

* Example

  ```haskell
  runEval $ eval (parse "let x=1+2; x+x")
  > 6 ```

## Interpreter 2:  the state monad transformer
[code](https://bitbucket.org/russo/afp-code/src/HEAD/L6/Interpreter2.hs?at=master&fileviewer=file-view-default)

* We want to extend our language of expression with *mutuable references*, e.g., we
  would like to run a program like `let r = new 7; !r+!r`

* We add new constructors in our expressions language for creation, reading, and
  writing of references.

  ```haskell
  data Expr = Lit Integer
            | Expr :+: Expr
            | Var Name
            | Let Name Expr Expr
            | NewRef Expr         -- new
            | Deref Expr          -- new
            | Expr := Expr        -- new ```

   Expression `NewRef e` creates a fresh reference which initially contains the
   value denoted by `e`.

   Expression `Deref e` denotes the value stored by the
   reference denoted by `e`.

   Expression `e1 := e2` changes the value stored in
   the reference denoted by `e1` by the value denoted by `e2`.

* The interpreter needs a notion of *memory* or *store* to keep the values
  stored in references

* This store is mutuable, since references are mutuable too!

* We represent it as a mapping from *memory locations* to *values*

  ```haskell
   type Ptr    = Value
   data Store = Store { nextPtr :: Ptr
                      , heap    :: Map Ptr Value
                      } ```

   A Store is then a mapping from a value denoting a memory location to the
   value stored at that location.

   For instance, the following mapping

   ```haskell
    Map.insert 2 42 (Map.insert 1 7 (Map.insert 0 101 Map.empty)) ```

   denotes a memory with three references: at location `0` with value `101`, at
   location `1` with value `7`, and at location `2` with value `42`.

   We also remember the next unused pointer as part of the store (useful when
   allocating a new reference).

   ```haskell
   emptyStore :: Store
   emptyStore = Store 0 Map.empty ```

* One alternative is to hard-wired it into the interpreter

  ```haskell
  eval :: Store -> Expr -> Eval Value ```

  This means that there would be some *plumbing* to pass the store (memory)
  between recursive calls (recall lecture 3).

* Instead, we use the monad transformer `StateT` to add a *mutuable state* into
  the `Eval` monad!

  <div class="container">
     <img class="img-responsive col-md-8"
       src="./assets/img/monadt_st.png">
  </div>

  In other words, `StateT` takes a monad `m` and returns a monad which contains
  a mutuable state of certain type `s` and two operations: `get` and `put` --
  such monads are called `MonadState s`. (Actually, they have one more operation
  ([`state`](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Lazy.html#g:1))
  but we will not describe it here)

   ```haskell
   data StateT s m a

   evalStateT :: Monad m => StateT s m a -> s -> m a

   get        :: MonadState s m => m s
   put        :: MonadState s m => s -> m () ```

* The monad `Eval` is then responsible for the *plumbing* required to pass
  around the store.

  ```haskell
   newtype Eval a = MkEval (StateT Store (ReaderT Env Identity) a)
     deriving (Functor, Applicative,
               Monad, MonadState Store, MonadReader Env) ```

   <div class="container">
      <img class="img-responsive col-md-10 "
        src="./assets/img/monad_st_r_id.png">
   </div>

* Alternatively, we could have defined an state-reader monad from
  scratch, i.e., not using the monad transformers

  ```haskell
  data Eval a = MkEval (\Store -> Env -> (a, Store)) ```

  <div class="alert alert-info">
  **Exercise**: Implement a "state-reader monad" directly
  ```haskell
   newtype MyMonad s e a = MyMonad {runMyMonad :: s -> e -> (a,s)}

   instance Monad (MyMonad s e) where
   return = returnMyMonad
   (>>=)  = bindMyMonad

   returnMyMonad :: a -> MyMonad s e a
   returnMyMonad x = MyMonad $ \s -> \ e -> (x, s) ```
  </div>

  While it works, the price to pay is modularity.

* Let us define some operations for the interpreter based on `get` and `put`

  *Creation of a new reference*: it implies to modify both the next available
  memory location and the heap.

  ```haskell
   newRef :: Value -> Eval Ptr
   newRef v = do store <- get
                 let ptr      = nextPtr store
                     new_ptr  = 1 + ptr
                     newHeap  = Map.insert ptr v (heap store)
                 put (Store new_ptr newHeap)
                 return ptr ```

   *Getting the value of a reference*: it implies to look up a memory location
    in the store. We crash with our own "segfault" if given a non-existing
    pointer. (Observe that the state has not been changed.)

   ```haskell
   deref :: Ptr -> Eval Value
   deref p = do st <- get
                let h = heap st
                case Map.lookup p h of
                     Nothing -> fail ("Segmentation fault: "++show p++" is not bound")
                     Just v  -> return v ```

   *Updating a reference*: it implies to change a value in the store.

    ```haskell
    (=:) :: MonadState Store m => Ptr -> Value -> m Value
    p =: v = do store <- get
                let updt_heap = Map.adjust (const v) p (heap store)
                put (store {heap = updt_heap})
                return v
    -- Map.adjust :: (Ord k) => (a -> a) -> k -> Map k a -> Map k a ```

   Observe that `(=:)` has no effect if the reference does not exist.

   <div class="alert alert-info">
   **Exercise**: Maybe that is not the best semantics. What would it be a better one?
   </div>

* We define the evaluation of expressions for the new cases with the functions
  described above.

   ```haskell
   eval :: Expr -> Eval Value
   eval (Lit n)        = return n
   eval (a :+: b)       = (+) <$> eval a <*> eval b
   eval (Var x)        = lookupVar x
   eval (Let n e1 e2) = do v <- eval e1
                           localScope n v (eval e2)
   eval (NewRef e)     = do v <- eval e
                            newRef v
   eval (Deref e)      = do r <- eval e
                            deref r
   eval (pe := ve)     = do p <- eval pe
                            v <- eval ve
                            p =: v ```

    As before, we do not need to change the definition for the old
    constructors.

* We can test it

  ```haskell
  > runEval $ eval $ parse "let p=new 7; !p"
  7 ```

* Unfortunately, our expression language supports pointers arithmetic (like C),
  so a pointer might dereference another one! -- a recipe for disaster!

  ```haskell
  > runEval $ eval $ parse "let p=new 1; let q=new 1738; !(p+1)"
  1738 ```

* How are we going to fix it?

  - Restricting the grammar, i.e., `!` only accepts variables names (not
    expressions)
  - Small type-system (over killing here)

* What other thing can go wrong with our interpreter?

  Expressions might refer to unbound variables / references.

  ```haskell
  > runEval $ eval $ parse "q + 1"
  *** Exception: Variable q not found. ```

  ```haskell
  > runEval $ eval $ parse "!q"
  *** Exception: Variable q not found. ```

  We need some exception handling mechanism into the language of expressions.

## Summary

- Programs often handle more than one effect

- Monad transformer are type-level functions which allow to extend existing
  monads with additional side-effects

  * The deriving mechanisms in Haskell is quite powerful and promotes
    re-utilization of code

  * Monad transformers are not essential and you can create your own monad with
    all the effects that you need. The problem? Modularity, i.e., code rewritten
    if you need to change your monad to add a new effect.
