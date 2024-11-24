# Untyped Lambda Calculus

This is a simple implementation of the untyped [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) in Rust.
It is extended with variable bindings to terms.

## Example

```lisp
((\x.(\x.x))x)
```

Is a valid term that evaluates to `λx.x`.

```lisp
(((λx.λy.x y) (λz.z)) (λw.w))
```

Simply becomes `λw.w`.

```lisp
((λx.(x x)) (λx.(x x)))
```

Is a non-terminating term.

```list
f = λx.λy.x y
g = λz.z
h = λw.w
((f g) h)
```

Reduces to `λz.z`.

## Usage

Start a REPL by running the following command:

```bash
./lambda_calc
```

Or run a file with lambda calculus terms:

```bash
./lambda_calc examples/identity.lc
```
