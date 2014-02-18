mlist
=======

Provides a data type `MList`, which enables the lazy computation of lists of monadic values. In certain cases, it is not possible to generate lists of monadic values in a lazy fashion. A simple example:

```haskell
ioSum :: IO [Int]
ioSum = ioSum' 0
  where ioSum' acc = do newValue <- getMeasurement
                        rest <- ioSum' (acc + newValue)
                        return $ newValue : rest
```

`ioSum` continuously reads in some input and sums the values. We would expect
to be able to get, say, the first 10 values of this stream:

```haskell
> ioSum >>= return . take 10
(endless loop)
```

The semantics of `>>=` of preclude a lazy evaluation of the list: since `rest <- ioSum' $ acc + newValue` is computed before `return $ newValue : rest`, `ioSum` always enters an infinite loop, without returning a single value. This phenomenon occurs as soon as `>>=` is used, no matter the function's construction. For instance, we could try to rewrite `ioSum` using fold, but the problem would persist:

```haskell
ioSum :: IO [Int]
ioSum = foldl (\acc i -> do {(x:xs) <- acc; i' <- i; return $ (x + i') : x : xs}) (return [0]) (repeat getMeasurement)
```

```bash
> ioSum >>= return . take 10
(endless loop)
```

We can solve this problem by introducing a modified version of the list, called an `MList`. Whereas the the list type is `List a = Nil | a : (List a)`, the type of an MList is `MList m a = (Monad m) => MNil | a :# m (MList m a)`. A regular list can only contain monadic computation if its type is `[m a]` or `m [a]`. An MList, on the other hand, can limit the monadic computations to its tail, while its pure head can be accessed without problems.

With MList, we can rewrite the example:

```haskell
data MList m a = MNil | a :# m (MList m a)

takeML :: Monad m => Int -> MList m a -> MList m a
takeML n xs = ...

ioSum :: IO (MList IO Int)
ioSum = ioSum' 0
  where ioSum' acc = do newValue <- getMeasurement
                        return $ newValue :# ioSum' (acc + newValue)
```

Lazy evaluation now works as intended:

```haskell
> ioSum >>= takeML 10 ioSum
[3, 9, 24, 46, 88, 90, 93, 121, 452, 530]
```

Contact
-------

Comments/contributions/bug reports/compaints are welcome. Do feel free to report an issue or write a mail!
