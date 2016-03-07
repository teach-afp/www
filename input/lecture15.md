# Type-based modeling & looking back!

## Type-based modeling

* In this lecture, we will see some advanced type-system features for Haskell
* Most advanced type-system features are introduced for two purposes
  - Type *more* well-behaved programs
  - Rule out *more* well-typed but bad-behaved programs

  <div class = "alert alert-info">
  GRAPHICS
  </div>

## Typing *more* well-behaved programs

* In Haskell, the addition has the following signature

  ```haskell
  (+) :: Num a => a -> a -> a ```

  This allows any numeric type to be used for addition *as long as* both
  arguments have the *same type*.

* In math, it is common to allow addition of, for
  example, integers and real numbers.

* Defining a more flexible addition is a nice little exercise to introduce
  *associated types* (type families).

* In principle, there is no reason for not typing the following expression

  ```haskell
  (1 :: Int) + (2.5 :: Float) ```

  A *good* implementation for "this" `(+)` requires to "cast" the left-side
  argument to a float and then perform the sum.

## Associated types

* What is the type of `(+)`?

  ```haskell
  (1 :: Int) + (2.5 :: Float) ```

  In this case, we have that

  ```haskell
  (+) :: Int -> Float -> Float ```

  If we have other arguments,

  ```haskell
  (1.5 :: Double) + (2.1 :: Float) ```

  Then, we have that

  ```haskell
  (+) :: Double -> Float -> Double ```

* The resulting type of `(+)` depends on the type of the arguments.

  When there are dependencies, there are chances to use functions!

  If you think `F` as a *function on types*, we can give the type signature of
  `(+)` as follows.

  ```haskell
  (+) :: a -> b -> F a b ```

* An associate type is a function at the level of types

   ```haskell
   class Add a b where        -- uses MultiParamTypeClasses
     type F a b               -- uses TypeFamilies as well
     add :: a -> b -> F a b ```

   Roughly speaking, functions at the type level require the `TypeFamilies`
   extension.

* Where do we give the definition of `F`?

  In the type class instances!

  ```haskell
  instance Add Integer Double where
    type F Integer Double = Double  -- defining F
    add x y = fromIntegral x + y

  instance Add Double Integer where
    type F Double Integer = Double  -- defining F
    add x y = x + fromIntegral y ```

  As for `(+)`, we define `add` for arguments of the same type.

  ```haskell
  instance (Num a) => Add a a where -- uses FlexibleInstances
    type F a a = a
    add x y = x + y ```

  We can even define addition between a number and a list (of integer, float, etc.).

  ```haskell
  instance (Add Integer a) =>
           Add Integer [a] where    -- uses FlexibleContexts
    type F Integer [a] = [F Integer a]
    add x ys = map (add x) ys ```

* Now, we can *safely* add elements of different types

  ```haskell
  test = add (3::Integer) (4::Double)

  aList :: [Integer]
  aList =  [0,6,2,7]

  test2 :: [Integer]
  test2 = add (1::Integer) aList

  test3 :: [Double]
  test3 = add (38::Integer) [1700::Double] ```
