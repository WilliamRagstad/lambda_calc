# Untyped Lambda Calculus

This is a simple implementation of the untyped [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) in Rust.
It is extended with variable assignments to terms.

## Usage

```bash
./lambda_calc                        # Start REPL
./lambda_calc examples/identity.lc   # Run a file
```

To display more detailed information, use `--help`.

## Example

```hs
(\x.\x.x) x
```

Is a valid term that evaluates to `λx.x` by applying the identity function `λx.λx.x` to the argument `x`.

```hs
((λx.λy.x y) (λz.z) (λw.w))
```

Simply becomes `λw.w`.

```hs
(λx.(x x)) (λx.(x x))
```

The above term is called the Omega combinator and reduces to itself. Resulting in a non-terminating term.

```hs
F = λx.λy.(x y);
G = λz.z;
H = λw.w;
F G H
```

Uses assignments to simplify the last term `F G H`, and reduces to `λz.z`.

## Boolean Logic

Boolean logic can be encoded using Church booleans:

```hs
True  = λtrue.λfalse.true
False = λtrue.λfalse.false
```

Here the parameter names `true` and `false` are used to represent the two possible outcomes of a logical operation, and does not have any meaning beyond that.

And logical operations:

```hs
Not = λa.(a False True)
And = λa.λb.(a b False)
Or  = λa.λb.(a True b)
If  = λa.λt.λf.(a t f)
```

> [!NOTE]
> Try evaluating the following terms yourself
>
> ```hs
> Not True
> Or False True
> And True False
> ```

## Arithmetics

Natural numbers can be encoded using Church numerals as shown below.
Imagine `f` as an action like "take a step forward," and `x` as your starting position on a number line.

```hs
0 = λf.λx.x
1 = λf.λx.(f x)
2 = λf.λx.(f (f x))
3 = λf.λx.(f (f (f x)))
4 = λf.λx.(f (f (f (f x))))
5 = λf.λx.(f (f (f (f (f x)))))
```

And arithmetic operations:

```hs
Succ = λn.λf.λx.(f (n f x))
Add  = λm.λn.λf.λx.((m f) (n f x))
Mul  = λm.λn.λf.λx.((m (n f)) x)
```

> [!NOTE]
> Try evaluating the following terms yourself
>
> ```hs
> Succ 0
> (Add 1) 2
> (Mul 2) 3
> ((If True) 2) 3
> ```

> [!TIP]
> Use the `--verbose` flag to see the evaluation steps.
>
> ```hs
> > (Add 1) 2
> (((λm.λn.λf.λx.((m f) ((n f) x))) (λf.λx.(f x))) (λf.λx.(f (f x))))
> ((λn.λf.λx.(((λf.λx.(f x)) f) ((n f) x))) (λf.λx.(f (f x))))
> λf.λx.(((λf.λx.(f x)) f) (((λf.λx.(f (f x))) f) x))
> λf.λx.((λx.(f x)) ((λx.(f (f x))) x))
> λf.λx.(f ((λx.(f (f x))) x))
> λf.λx.(f (f (f x))) -- Result: 3
> ```

## Comparison

Comparison can be implemented using the Church encoding of natural numbers.
The `IsZero` function checks if a number is zero, and the `Eq` function checks if two numbers are equal.

```hs
IsZero = λn.((n λx.False) True)
Eq     = λm.λn.((And (IsZero ((Add m) n))) (IsZero ((Add n) m)))
```

## Combinators

Combinators are lambda terms that do not have free variables.
Common fixed-point combinators include: identity `I`, substitution `S`, constant `K`, mockingbird `M`, omega `O`, and `Y`, among others.

```hs
I = λx.x
S = λx.λy.λz.((x z) (y z))
K = λx.λy.x
M = λf.((f f) f)
O = λx.(x x)
Y = λf.((λx.(f (x x)) λx.(f (x x))))
```

> [!NOTE]
> Try evaluating the following terms yourself
>
> ```hs
> ((S K) K) I -- Result: I
> (M M) M     -- Result: Non-terminating
> ```

## Recursion

Recursion can be implemented using the Y combinator, which is a fixed-point combinator that allows for the definition of recursive functions.
The Y combinator is defined as follows:

```hs
Y = λf.((λx.(f (x x)) λx.(f (x x))))
```

The Y combinator can be used to define recursive functions as shown below:

```hs
Fact = (Y (λf.λn.(((If (IsZero n)) 1) ((Mul n) (f (Pred n))))))
```

Where `IsZero` and `Pred` are defined as:

```hs
IsZero = λn.((n λx.False) True)
Pred   = λn.((n (λp.λz.z ((Add p) 0))) 0)
```

## Fundamentals

The lambda calculus is a formal system for expressing computation based on function abstraction and application using variable bindings.
It is a universal model of computation that can express any computation that can be performed by a Turing machine.

The reduction operation that drives computation is mainly $β$-reduction, which is the application of a function to an argument.

$$
(\lambda x.M)\ N \quad\rightarrow\quad M[x:=N]
$$

Where $M[x:=N]$ is the result of replacing all free occurrences of $x$ in the body of the abstraction $M$ with the argument expression $N$.
The second fundamental operation is $α$-conversion ($`(\lambda x.M[x]) \rightarrow (\lambda y.M[y])`$), which is the renaming of bound variables in an expression to avoid name collisions.

## Language Extensions

### Application Ergonomics

The application of a function to an argument is denoted by placing the function and argument next to each other without any delimiters: `f x`.

To make the syntax more ergonomic, application can take any number of arguments separated by whitespace.

```hs
A B C D E == ((((A B) C) D) E)
```

### Environment $\Gamma$

The interpreter will always reduce terms to their simplest form, aka normal form.
This is done **symbolically** by applying the beta reduction rules until no more reductions are possible.
However, this allows for a nice interactive way of working, as *you can refer to non-existing terms* and still get a result.

```hs
> And = λa.λb.(a b False) -- Define And
> And True False          -- Attempt 1
(((True False)) False)    -- Notice: True and False are undefined but we still get a result

> True  = λtrue.λfalse.true  -- Define True
> False = λtrue.λfalse.false -- Define False
> And True False          -- Attempt 2
λtrue.λfalse.false        -- Result: False
```

The implementation is extended with variable assignments to terms.
This allows for the definition of terms that can be used later in the evaluation of other terms using an environment $\Gamma$ mapping names to terms.

```hs
(((λx.x) (λx.x)) (λx.x))
```

Or the same with assignments:

```hs
Id = λx.x
((Id Id) Id)
```

The term `Id` can now be used in other terms to simplify expressions.
Both terms evaluate to `λx.x`.

### REPL Commands

The REPL also has commands to load files and display the current environment.

```bash
> :load ./examples/std.lc
> :help  # Display help
```
