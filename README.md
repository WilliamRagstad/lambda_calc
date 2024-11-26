# Untyped Lambda Calculus

This is a simple implementation of the untyped [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) in Rust.
It is extended with variable assignments to terms.

## Example

```hs
((\x.(\x.x))x)
```

Is a valid term that evaluates to `λx.x`.

```hs
(((λx.λy.x y) (λz.z)) (λw.w))
```

Simply becomes `λw.w`.

```hs
((λx.(x x)) (λx.(x x)))
```

The above term is called the Omega combinator and reduces to itself. Resulting in a non-terminating term.

```hs
F = λx.λy.(x y)
G = λz.z
H = λw.w
((F G) H)
```

Uses assignments to simplify the last term `(F G) H`, and reduces to `λz.z`.

### Data Encoding

Boolean logic can be encoded using Church booleans:

```hs
True  = λtrue.λfalse.true
False = λtrue.λfalse.false
```

Here the parameter names `true` and `false` are used to represent the two possible outcomes of a logical operation, and does not have any meaning beyond that.

And logical operations:

```hs
Not = λb.((b False) True)
And = λa.λb.((a b) False)
Or  = λa.λb.((a True) b)
```

> [!NOTE]
> Try evaluating the following terms yourself
>
> ```hs
> Not True
> (Or False) True
> (And True) False
> ```

#### Numbers

Natural numbers can be encoded using Church numerals as shown below.
Imagine `f` as an action like "take a step forward," and `x` as your starting position on a number line.

```hs
0 = λf.λx.x
1 = λf.λx.(f x)
2 = λf.λx.(f (f x))
3 = λf.λx.(f (f (f x)))
```

And arithmetic operations:

```hs
Succ = λn.λf.λx.(f ((n f) x))
Add  = λm.λn.λf.λx.((m f) ((n f) x))
Mul  = λm.λn.λf.λx.((m (n f)) x)
```

> [!NOTE]
> Try evaluating the following terms yourself
>
> ```hs
> Succ 0
> (Add 1) 2
> (Mul 2) 3
> ```

## Usage

Start a REPL by running the following command:

```bash
./lambda_calc
```

Run a file:

```bash
./lambda_calc examples/identity.lc
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

## Extension

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
