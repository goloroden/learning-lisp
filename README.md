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

As `nil` itself, they also are of type `NULL`.

### Character literals

Characters are represented by using the prefix `#\`. Hence, e.g. the character `a` becomes `#\a`, as in

```lisp
(princ #\a)
;; => a
```

Additionally, there are a few special character literals, such as:

- `#\newline`
- `#\space`
- `#\tab`

## Data mode

To switch Lisp into data mode you have to use *quoting*. This can either be done by using the `quote` command or by prefixing a list with the `'` character. Hence the following two lists are equivalent:

```lisp
(quote 2 3 5 7 11)
'(2 3 5 7 11)
```

There is also *quasi-quoting* which is uses the ` character as a prefix, but allows to switch back to code mode using a `,`:

```lisp
\`(2 3 ,(+ 2 3) 7 11)
```
