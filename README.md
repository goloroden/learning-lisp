# learning-lisp

learning-lisp contains my notes on Lisp.

## Literals

### Boolean literals

Lisp contains two literals that are typically used in a boolean context:

- `t` is of type `BOOLEAN` and represents `true`
- `nil` is of type `NULL` and represents `false`

#### Falsy values

Besides `nil` there are a number of literals that are equivalent to `false`:

- `'nil`
- `()`
- `'()`

They all are of type `NULL`.

### Character literals

Characters are represented by using the prefix `#\`. Hence, e.g. the character `a` becomes `#\a`. Additionally, there are a few special character literals, such as:

- `#\newline`
- `#\space`
- `#\tab`
