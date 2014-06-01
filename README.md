# learning-lisp

learning-lisp contains my notes on Lisp.

## Comments

In Lisp a comment is started by the `;` character. Depending on where the comment is placed, the number of `;` characters vary:

- Use one semicolon for a comment at the end of a line.
- Use two semicolons for a comment that is inlined with the code.
- Use three semicolons for a comment that document code blocks and start in column 1.
- Use four semicolons for titles and introductory information on a code file.

The following sample shows how comments should be used:

```lisp
;;;; This sample implements a simple hello world application.

;;; Define the hello-world function.
(defun hello-world ()
  ;; Print the string hello world to the console.
  (princ "Hello world!")) ; Might use prin1 instead.

(hello-world) ; Finally, call the hello-world function.
```

## Literals

### Boolean literals

Lisp contains two literals that are of type `BOOLEAN` and hence are typically used in a boolean context:

- `t` represents `true`
- `nil` represents `false`

#### Values evaluating to false

Besides `nil` there are a number of expressions that also evaluate to `false` and are commonly referred to as equivalents to `nil` within conditions:

- `'nil`
- `()`
- `'()`

Please note that not all of them are literals, especially the quoted ones.

### Character literals

Characters are represented by using the prefix `#\`. Hence, e.g. the character `a` becomes `#\a`, as in

```lisp
(princ #\a) ; => a
```

Additionally, there are a few special character literals, such as:

- `#\newline`
- `#\space`
- `#\tab`

## Data mode

To switch Lisp into data mode you have to use *quoting*. This can either be done by using `quote` or by prefixing a list with the `'` character. Hence the following two lists are equivalent:

```lisp
(quote (2 3 5 7 11))
'(2 3 5 7 11)
```

There is also *quasi-quoting* which uses the `` ` `` character as a prefix, but allows to switch back to code mode using a `,`:

```lisp
`(2 3 ,(+ 2 3) 7 11)
;; => (2 3 5 7 11)
```

## Variables

### Defining global variables

To define global variables use `defparameter` and `defvar`. They differ in terms of repeatability: While a variable defined with `defparameter` can be re-defined, variables defined with `defvar` can't.

In Lisp, it is common to surround global variable names with so-called *ear-muffs*, that is, with a `*` as prefix and suffix:

```lisp
(defparameter *the-answer-to-everything* 42)
```

### Defining local variables

To define local variables use `let`. You can define one or more variables, and they are only visible in the body of `let`:

```lisp
(defun add ()
  (let ((x 23)
        (y 42))
    (+ x y)))
```

### Assigning values

To assign a value to a variable use `setf`:

```lisp
(setf *foo* 23)
```

An expression may be used for the value to be assigned. Hence, the sample above may be rewritten as:

```lisp
(setf *foo* (+ 10 13))
```

## Functions

### Defining global functions

To define a global function use `defun`. Specify the name of the function as well as its parameters and its body.

```lisp
(defun add (x y)
  (+ x y))
```

If a function doesn't have any parameters, simply specify an empty list.

### Defining local functions

To define local functions use `flet`. Basically it works like `let`, except that it defines functions instead of variables.

```lisp
(defun calculate ()
  (flet ((add (x y)
              (+ x y))
         (mul (x y)
              (* x y)))
    (mul 2 (add 3 4))))

(calculate)
;; => 14
```

Alternatively, you may use `labels`. It works in exactly the same way as `flet`, but defined functions can use the ones defined earlier. In contrast, when using `flet`, the functions are only accessible from `flet`'s body.

### Defining lambda functions

To define a lambda function use `lambda`.

```lisp
(mapcar (lambda (x) (* x x)) '(1 2 3 4 5))
;; => (1 4 9 16 25)
```

## Calculations

### Basic arithmetic operations

First of all, Lisp supports the four basic arithmetic operations: `+`, `-`, `*` and `/`, that are used in prefix notation:

```lisp
(* 2 (+ 3 4)) ; => 14
```

In contrast to other languages, the codomain is not limited, i.e. numbers may become arbitrarily large. Additionally, Lisp supports division with rational numbers, such as:

```lisp
(/ 1 3) ; => 1/3
```

### Shortcut arithmetic operations

If you just want to add or subtract one from a given value, you may use `1+` and `1-`. Hence, the following two lines of code are equivalent:

```lisp
(- 7 1) ; => -6
(1- 7)  ; => -6
```

### Arithmetic shifting

To do arithmetic shifting use `ash`. It requires an expression and the number of bits to shift to the left or to the right. Positive values shift to the left, while negative values shift to the right:

```lisp
(ash 16 1)  ; => 32
(ash 16 3)  ; => 128
(ash 16 -1) ; => 8
(ash 16 -3) ; => 2
```

### Power and root

To calculate the power of a value use `expt` and provide the base as well as the exponent. In contrast, to calculate the square root use `sqrt`:

```lisp
(expt 2 4) ; => 16
(sqrt 16)  ; => 4
```

## Predicate functions

In Lisp there are a number of pre-defined predicate functions. They end in `p`, or - if the function itself already contains a hyphen - in `-p`.

### Predicates on numbers

To test whether a number is even or odd, use `evenp` and `oddp`:

```lisp
(evenp 42) ; => t
(evenp 23) ; => nil

(oddp 42)  ; => nil
(oddp 23)  ; => t
```

### Predicates on characters

To test whether a character is of a specific class of characters, use `alphanumericp` and `digit-char-p`:

```lisp
(alphanumericp #\a) ; => t
(alphanumericp #\1) ; => t
(alphanumericp #\_) ; => nil

(digit-char-p #\a)  ; => nil
(digit-char-p #\1)  ; => t
(digit-char-p #\_)  ; => nil
```

## Higher-order functions

First of all, to access a function by name, you need to use `function` or its short cut `#'`. Hence the two following expressions are equivalent:

```lisp
(function evenp)
#'evenp
```

### Mapping

To map a list to a function, use `mapcar` or `mapc`, depending on whether you want it to return the mapped list (`mapcar` does, `mapc` doesn't and instead returns the original list):

```lisp
(mapcar (lambda (x) (* x x)) '(1 2 3 4 5))
;; => (1 4 9 16 25)

(mapc (lambda (x) (* x x)) '(1 2 3 4 5))
;; => (1 2 3 4 5)
```
