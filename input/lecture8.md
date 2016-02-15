# Monad transformers II: composition of error and state transformers

## The except (error) monad transformer

* In our interpreter, expressions might refer to unbound variables / references.

  ```haskell
  > runEval $ eval $ parse "q + 1"
  *** Exception: Variable q not found. ```

  ```haskell
  > runEval $ eval $ parse "!q"
  *** Exception: Variable q not found. ```

  We need some exception handling mechanism into the language of expressions.

* We add a data type which captures the type of exceptions (errors) occurring in our
  interpreter

  ```haskell
  data Err = SegmentationFault
           | UnboundVariable String
           | OtherError String ```


* First, we could explicitly indicate that errors might happen in the interpreter

  ```haskell
   eval :: Expr -> Either Err Value ```

  This means that there would be some *plumbing* to check if every recursive
  call has finished succesfully, and if it is not the case, fail the whole
  computation.

* Instead we use a monad transformer!

   <div class="container">
      <img class="img-responsive col-md-10 "
        src="./assets/img/exceptT.png">
   </div>

  `ExceptT` takes a monad `m` and returns a monad which support
  two operations: `throwError` and `catchError` -- such monads are called `MonadError`.

  <div class="alert alert-info">
   A monad obtained by the transformer `ExceptT` is a`MonadError`!
  </div>

   ```haskell
   data ExceptT e m a

   runReaderT :: ReaderT s m a -> (s -> m a)

   throwError :: MonadError e m => e -> m a
   catchError :: MonadError e m => m a -> (e -> m a) -> m a ```


## Composition of effects

* We add an error monad (called except monad in GHC) to our evaluation monad
  `Eval`.

* We can understand the interaction between the state monad and the error monad
  by looking at their implementations (i.e., types).

* It matters whether we stick the error monad on the outside or the inside of
  the state monad.

* With `ExceptT` on the **outside**, we will represent computations as

   <div class="container">
      <img class="img-responsive col-md-10 "
        src="./assets/img/m_err_st.png">
   </div>


  Using types, we will have computations of the form

  ```haskell
  m_st (Either err a) ```

  where `m_st` is an state monad.

  Roughly speaking, this type is like having

  ```haskell
  s -> (Either err a, s) ```

  Since the state is hidden inside `m_st`, it is not affected by whether we return
  `Right a` or `Left err`.

  <div class = "alert alert-info">
   State changes will not be rolled back when there's an
   exception.
  </div>

  <div class = "alert alert-info">
   When there is an exception, the state information is there!
  </div>


* Instead, if we place it on the inside, we have that

   <div class="container">
      <img class="img-responsive col-md-10 "
        src="./assets/img/m_st_err.png">
   </div>

* With `ExceptT` on the **inside**, i.e., adding a state monad on top of an
  error monad, computations will be represented as

  Using types, we will have computations of the form

   ```haskell
   s -> m_ex (a, s) ```

   where `m_ex` is an exception monad.

   Roughly speaking, this type is similar to have

    ```haskell
    s -> Either e (a, s) ```

   <div class = "alert alert-info">
    If a computation fails, we lose any changes to the state made by the failing
    computation
   </div>

   Observe that the state is just passed as a return value in the underlying monad.

## Interpreter 3: `ExceptT` on the inside
[code](https://bitbucket.org/russo/afp-code/src/HEAD/L8/Interpreter3.hs?at=master&fileviewer=file-view-default)

* We apply the monad transformer *on the inside* and wrapped it with a reader
  and state monad.

  ```haskell
  newtype Eval a = MkEval (StateT Store
                                  (ReaderT Env
                                           (ExceptT Err Identity)) -- new
                                  a )

    deriving (Functor, Applicative,
              Monad, MonadState  Store
                   , MonadReader Env
                   , MonadError  Err -- new
                   ) ```

* The run function then includes `runExceptT` right after the reader's run
  function

   ```haskell
   runEval :: Eval a -> Either Err a      -- new type!
   runEval (MkEval st) = runIdentity
                         (runExceptT     -- new
                           (runReaderT
                               (evalStateT st emptyStore)
                           emptyEnv)
                         ) ```

* We adapt the interpreter's auxiliaries functions where they can raise errors

  *Looking a variable in the environment*

  ```haskell
  lookupVar :: Name -> Eval Value
  lookupVar x = do
    env <- ask
    case Map.lookup x env of
      Nothing -> throwError (UnboundVariable x) -- new
      ... ```

  *Deferencing a location which does not exists*

  ```haskell
   deref :: Ptr -> Eval Value
   deref p = do st <- get
                let h = heap st
                case Map.lookup p h of
                  Nothing -> throwError SegmentationFault -- new
                  ... ```

* We finally write our interpreter (the only new case is `Catch`)

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
                             p =: v
    eval (Catch e1 e2)  = catchError (eval e1) (\_err -> eval e2) ```

* We can recover from errors

  ```haskell
  testExpr2 = parse "(try !p catch 0)+1738" ```

  ```haskell
  > runEval $ eval $ testExpr2
  1738 ```

* What does it happen with the side-effects when an error occurs?

  ```haskell
  testExpr3 = parse "let one = new 1; \
                   \ let dummy = (try ((one := 2) + !7) catch 0); \
                   \ !one" ```

  What is the value of `one`?

* Let us run it first

  ```haskell
  > runEval $ eval $ testExpr3
  Right 1 ```

  The side-effects (namely, `one := 2`) are reverted if an error occurs!

* To explain what happens in more detail, let us see the program

   <div class="container">
      <img class="img-responsive col-md-10 "
        src="./assets/img/m_st_err_ex.png">
   </div>

  The expression `((one := 2) + !7)` can be thought as a function which
  takes an store and returns always an error, i.e., a function semantically
  equivalent to `\s -> Left SegmentationFault`.

  Then, due to the try-catch statement, the expression
  `(try ((one := 2) + !7) catch 0)` catches the error and then produces
  a function semantically equivalent to `\s -> Right (Lit 0, s)`. Observe that
  it returns the state before the effect `one := 2`.


## Interpreter 4: `ExceptT` on the outside
[code](https://bitbucket.org/russo/afp-code/src/HEAD/L8/Interpreter4.hs?at=master&fileviewer=file-view-default)

* We apply the monad transformer *on the outiside*

  ```haskell
  newtype Eval a = MkEval (ExceptT Err                              -- new
                                   (StateT Store
                                           (ReaderT Env Identity))
                                   a)

    deriving (Functor, Applicative,
              Monad, MonadState  Store
                   , MonadReader Env
                   , MonadError  Err -- new
                   ) ```

* Since `ExceptT` is the outermost transformer, `runExceptT` is the first one to
  run

  ```haskell
  runEval (MkEval err) = runIdentity
                           (runReaderT
                              (evalStateT
                                          (runExceptT err) -- run first!
                                          emptyStore)
                          emptyEnv) ```

* The interpreter, and the auxiliaries' functions remain the same

* What does it happen with the side-effects when an error occurs?

  ```haskell
  testExpr3 = parse "let one = new 1; \
                   \ let dummy = (try ((one := 2) + !7) catch 0); \
                   \ !one" ```

  What is the value of `one`?

  ```haskell
  > runEval $ eval $ testExpr3
  Right 2 ```

  Observe that the program has recovered but the side-effects within the
  try-catch blocked were not rolled-back.

* To explain what happens in more detail, let us see the program

   <div class="container">
      <img class="img-responsive col-md-10 "
        src="./assets/img/m_err_st_ex.png">
   </div>

   The expression `((one := 2) + !7)` can be thought as a function which takes
   an store and returns an error but keeping the state at the point of the
   failure. You can think the computation as a function semantically similar to
   `\s -> (Left SegmentationFault, s')` where `s'` is the same store as `s`
   except that reference `one` is set to `2`.

   When the exception handler is executed, it takes the state from where `((one
   := 2 + !7))` left it, i.e., `s'`. So, the result of `!one` in the last line
   of the program is `2`.

## Which one is better? ExceptT *outside* or *inside*?

* That is application specific

* In our interpreter, the expected semantics for programs is obtained with
  `ExceptT` *on the inside*

* There are some side-effects which cannot be undone, e.g., launching a
  missile. For those effects, `ExceptT` on the outside might be suitable

## Interpreter 5: Adding I/O
[code](https://bitbucket.org/russo/afp-code/src/HEAD/L8/Interpreter5.hs?at=master&fileviewer=file-view-default)

* We want to add a new effect: I/O (for simplicity we only consider output)

* We will add a function to print out messages

   ```haskell
   let x = print \"hello\" ; 42 ```

* We will add the IO monad into the definition of `Eval`

  ```haskell
  newtype Eval a = MkEval
                      (StateT Store
                              (ReaderT Env (ExceptT Err IO)) -- new IO for Identity
                              a)

     deriving (Functor, Applicative,
               Monad, MonadState  Store
                    , MonadReader Env
                    , MonadError  Err
                    , MonadIO -- new
                    ) ```

    We replaced `Identity` by `IO`

* The evaluation monad `Eval` is, roughly speaking` of type

  ```haskell
  s -> r -> Either Err (IO (a,s)) ```

  This indicates that the except (error) monad still rolls back the effects on
  the store, but *not* the ones in `IO` (e.g., launch a missile).

* We make `Eval` an instance of the class `MonadIO`

  As a result, `Eval` can *lift* (cast) every IO-action into itself

  ```haskell
  liftIO :: MonadIO m => IO a -> m a ```

* We define the function to show an string

  ```haskell
  msg :: String -> Eval ()
  msg = liftIO . putStrLn ```

* We add a constructor to represent output commands

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
                            p =: v
   eval (Catch e1 e2)  = catchError (eval e1) (\_err -> eval e2)
   eval (Print m)      = do msg m  -- new
                            return 0 ```

* The following program produces an I/O effect before an exception is thrown

   ```haskell
    "let one = new 1; \
   \ let dummy = (try ( print \"hello!\" + (one := 2) + !7) \
   \             catch 0); \
   \ !one" ```

   If we run it, it produces the output and then it rolls back the effects on
   the store (i.e., `one := 2`)

   ```haskell
   > test3
   hello!
   Right 1 ```


## Implementing monad transformers

* So far, we have just seen how to use the monad transformers provided by GHC
  and the *magic* of `deriving`

* In case you designed your own monad to handle certain special effects, you
  might want to make a monad transformer for adding such effects to any monad!

* To be completed
