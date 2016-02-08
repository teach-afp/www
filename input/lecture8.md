# Monad transformers II: composition of error and state transformers

## Interpreter 3: the except (error) monad transformer

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
   eval :: Expr -> Either Err
