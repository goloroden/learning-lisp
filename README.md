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

### Common predicates

To test whether an expression is equivalent to `nil` use `null`:

```lisp
(null ())  ; => t
(null nil) ; => t
(null #\a) ; => nil
```

### Predicates on numbers

To test whether a number is even or odd, use `evenp` and `oddp`:

```lisp
(evenp 42) ; => t
(evenp 23) ; => nil

(oddp 42)  ; => nil
(oddp 23)  ; => t
```

To test whether a number is equal to zero use `zerop`:

```lisp
(zerop 0) ; => t
(zerop 1) ; => nil
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

### Mapping lists

To map a list to a function, use `mapcar` or `mapc`, depending on whether you want the mapped list to be returned or not (`mapcar` does, `mapc` doesn't and instead returns the original list):

```lisp
(mapcar #'evenp '(1 2 3 4 5))
;; => (nil t nil t nil)

(mapc #'evenp '(1 2 3 4 5))
;; => (1 2 3 4 5)
```

While `mapcar` rather conforms to the `map` function in functional programming, `mapc` is more like a `for each` loop and hence only useful for potential side-effects of the specified function.

Besides that there is also `maplist` which in each iteration processes the rest of the list:

```lisp
(maplist #'identity '(1 2 3))
;; => ((1 2 3) (2 3) (3))
```

### Complementing functions

Sometimes it is necessary to get the complement of a specific function, e.g. it might be useful to have a predicate that matches everything but a digit, so this would be the complement of `digit-char-p`.

To get the complement use `complement`:

```lisp
(mapcar (complement #'digit-char-p) '(#\a #\1 #\_))
;; => (t nil t)
```

### Invoking functions

To invoke a function use `apply` or `funcall`, depending on whether the parameters are given as a list or as separate values.

```lisp
(apply #'evenp '(42)) ; => t
(apply #'evenp '(23)) ; => nil

(funcall #'evenp 42)  ; => t
(funcall #'evenp 23)  ; => nil
```

### Lazy evaluation

If you want to evaluate an expression lazily, move it into a lambda function. Then you can move around this function and call it when needed. If such a function does not have any parameters, it is called a *nullary function* or a *thunk*:

```lisp
(defun run (thunk)
  (funcall thunk))

(run (lambda () (+ 23 42)))
;; => 65
```

## Running code

Sometimes code is given as data, e.g. when reading code from the console. To run such code use `eval`:

```lisp
(eval '(+ 23 42))
;; => 65
```

As usual, `eval` should be considered bad style and be used carefully.

## Working with lists

Lists in Lisp consist of *cons cells* that are linked to each other. Hence a list is, technically speaking, a single-linked list.

### Creating lists

To create a list use `cons` and specify the value of the cons cell as well as the next cons cell, or `nil` if it is the last element of a list:

```lisp
(cons 2 (cons 3 (cons 5 (cons 7 (cons 11 nil)))))
;; => (2 3 5 7 11)
```

Alternatively you may use `list`:

```lisp
(list 2 3 5 7 11)
;; => (2 3 5 7 11)
```

### Dotted lists and association lists

If you only use a single cons cell you can create a so-called *dotted list* respectively a *pair*:

```lisp
(cons 2 3) ; => (2 . 3)
```

Additionally, you may build lists of pairs. These are called *association lists* (or *alist*):

```lisp
'((1 . 1)
  (2 . 4)
  (3 . 9)
  (4 . 16))
```

### Getting the length of a list

To get the length of a list use `length`:

```lisp
(length '(2 3 5 7 11)) ; => 5
```

### Accessing list items

To get the first item from a list use `car`:

```lisp
(car '(2 3 5 7 11)) ; => 2
```

To get the rest from a list use `cdr`:

```lisp
(cdr '(2 3 5 7 11)) ; => (3 5 7 11)
```

Now you can combine these two. E.g., to access the third item from a list you first get the rest, then the rest of the rest, and then the first item, i.e. you nest `car`, `cdr` and `cdr`:

```lisp
(car (cdr (cdr '(2 3 5 7 11)))) ; => 5
```

Alternatively, you can combine those into a single call by merging the `a` and `d` characters:

```lisp
(caddr '(2 3 5 7 11)) ; => 5
```

This works for up to four levels.

### Combining and extending lists

To concatenate lists use `append` and provide an arbitrary number of lists:

```lisp
(append '(2 3) '(5 7 11))
;; => (2 3 5 7 11)
```

To add a new item to a list use `push`. Please note that `push` adds to the beginning of the list, not to its end:

```lisp
(defparameter *primes* '(3 5 7 11))
(push 2 *primes*)

*primes* ; => (2 3 5 7 11)
```

### Searching lists

To detect whether a given element is contained within a list use `member`. If the element is contained, `member` returns the rest of the list starting with the given element, otherwise it returns `nil`:

```lisp
(member 5 '(2 3 5 7 11)) ; => (5 7 11)
(member 4 '(2 3 5 7 11)) ; => nil
```

Please note that this also works if you are looking for `nil` itself, as in the case of success `member` returns a list containing `nil`.

Alternatively, you may use `find`. It returns the element itself if it is contained, rather than a list:

```lisp
(find 5 '(2 3 5 7 11)) ; => 5
(find 4 '(2 3 5 7 11)) ; => nil
```

Additionally you may specify a function that is used to get the key. This is useful if you want to search an `alist` for a given key:

```lisp
(find 'foo '((foo . bar) (baz . bas)) :key #'car)
;; => (foo . bar)
```

You may also use `find-if` if you want to describe the element being looked for using a function:

```lisp
(find-if (lambda (p) (> p 5)) '(2 3 5 7 11))
;; => 7
```

Finally, when working with `alist`s, you can also use `assoc` to get an element by its key, instead of using `find` with its `:key` parameter set to `#'car`:

```lisp
(assoc 'foo '((foo . bar) (baz . bas)))
;; => (foo . bar)
```

### Manipulating lists

If you want to replace elements of a list according to a given criterion use `substitute-if` and provide the replacement, a predicate and the list itself:

```lisp
(substitute-if 0 #'evenp '(1 2 3 4 5 6 7 8 9 10))
;; => (1 0 3 0 5 0 7 0 9 0)
```

Alternatively, you may want to remove the elements that match the predicate. For that use `remove-if`:

```lisp
(remove-if #'evenp '(1 2 3 4 5 6 7 8 9 10))
;; => (1 3 5 7 9)
```

## Working with characters and strings

### Getting the length of a string

To get the length of a string use `length`:

```lisp
(length "Hello world!") ; => 12
```

### Getting substrings

If you need a substring of a string use `subseq` and provide the start as well as the end index. Please note that they are zero-based:

```lisp
(subseq "Hello world!" 6 11)
;; => "world"
```

Sometimes you need to trim a string, i.e. remove one or more characters from its beginning, its end, or from both. For that use `string-trim`:

```lisp
(string-trim " " "   Hello world!   ")
;; => "Hello world!"
```

Please note that you can provide more than character by simply specifying them as a string in the first parameter:

```lisp
(string-trim " $" "$   Hello world!   ")
;; => "Hello world!"
```

### Extending strings

To concatenate multiple strings into a single one use `concatenate` and provide an arbitrary number of strings. Please note that you must specify the expected type of the result, i.e. `'string`:

```lisp
(concatenate 'string "Hello " "world!")
;; => "Hello world!"
```

### Manipulating strings

Sometimes you want to replace parts of a string with other text. For that you can use `substitute-if` and provide an appropriate predicate function:

```lisp
(substitute-if #\_ (complement #'alphanumericp) "Hello world!")
;; => "Hello_world_"
```

To convert characters to uppercase or lowercase, use `char-upcase` and `char-downcase`:

```lisp
(char-upcase #\a)   ; => A
(char-downcase #\A) ; => a
```

## Comparing values

When comparing values Lisp provides a number of possibilities. Some of them work only for specific types, others work for a variety of types as well.

### Identity

The simplest comparison possible is by using `eq`. It compares two values by reference, or - to put it differently - it compares their identity. Hence `eq` is primarily used to compare symbols:

```lisp
(eq 'a 'a) ; => t
(eq 'a 'b) ; => nil
```

You may also use `eql` which basically does the same as `eq`, but also handles numbers and characters. Anyway, number must be of the same type, either both integer or both decimal:

```lisp
(eql #\a #\a) ; => t
(eql 1.0 1.0) ; => t
```

### Equality for specific types

There are specific ways to compare specific types. E.g., is you want to compare numbers, no matter whether they are integers or decimals, use `=`:

```lisp
(= 1 1)   ; => t
(= 1 1.0) ; => t
```

Instead, if you need to compare characters use `char-equal`. Please note that this ignores the casing of the given characters:

```lisp
(char-equal #\a #\a) ; => t
(char-equal #\a #\A) ; => t
```

To compare not only single characters but complete strings use `string-equal`. Please note that this also ignores the casing:

```lisp
(string-equal "Hello" "Hello") ; => t
(string-equal "Hello" "HELLO") ; => t
```

### Equality for various types

Additionally to the comparisons mentioned before there are a number of more complex ones.

The most important one is `equal` that compares two values isomorphicly, i.e., as a rule of thumb you might say that things are considered to be equal when they look the same:

```lisp
(equal 'foo 'foo)     ; => t
(equal 1 1)           ; => t
(equal 1.0 1.0)       ; => t
(equal #\a #\a)       ; => t
(equal "abc" "abc")   ; => t
(equal '(2 3) '(2 3)) ; => t
```

If you rather want to compare for common meaning than for common looks, use `equalp` instead. It will also compare numbers of different types and ignore different casing when comparing characters and strings.

### Inequality

For numbers, just like `=`, you can also use `>`, `<`, `>=` and `<=`.

## Coercion

To cast a type into another one use `coerce` and provide the expression to cast as well as the demanded target type:

```lisp
(coerce "Hello" 'list)
;; => (#\H #\e #\l #\l #\o)

(coerce (#\H #\e #\l #\l #\o) 'string)
;; => "Hello"
```

## Conditionals

### Boolean relations

To merge multiple boolean values into a single you can either use `and` or `or`. Both provide shortcut evaluation:

```lisp
(and t t)   ; => t
(and t nil) ; => nil
(or t t)    ; => t
(or t nil)  ; => t
```

Please note that `and` and `or` also work with values of types other than `BOOLEAN`. The following lines show a few examples:

```lisp
(and 23 42)  ; => 42
(and nil 42) ; => nil
(or 23 42)   ; => 23
(or nil 42)  ; => 42
```

### When, unless and if

To run code only when a certain condition is met use `when`, provide the condition itself and one or more expressions to run:

```lisp
(defun is-prime (n)
  (when (find n '(2 3 5 7 11))
    'n-is-prime))
```

To run code when a condition is *not* met use `unless` instead:

```lisp
(defun is-not-prime (n)
  (unless (find n '(2 3 5 7 11))
    'n-is-not-prime))
```

Finally, when you need `when` and `unless` in combination use `if`. Please note that `if` does not support multiple expressions, hence both code paths must consist of a single expression:

```lisp
(defun is-prime (n)
  (if (find n '(2 3 5 7 11))
      'n-is-prime
      'n-is-not-prime))
```

### Executing multiple expressions

If you actually need to provide multiple expressions where only a single one is allowed use `progn`:

```lisp
(progn (princ "Hello ") (princ "world!"))
```

### Handling multiple conditions

From time to time you need to check multiple conditions and decide which branch to use. For these cases Lisp provides `cond` that takes pairs of conditions and expressions. Usually the last branch is specified by the condition `t`. Hence, its canonical form is:

```lisp
(cond
  (<condition> <expression> <...>)
  (<condition> <expression> <...>)
  (t <expression> <...>))
```

A real-life example may be an implementation of *fizz buzz*:

```lisp
(defun is-fizz-buzz (n)
  (cond ((and (zerop (mod n 3)) (zerop (mod n 5))) 'fizz-buzz)
        ((zerop (mod n 3)) 'fizz)
        ((zerop (mod n 5)) 'buzz)
        (t n)))

(is-fizz-buzz 15)
;; => fizz-buzz
```

Alternatively you can use `case` that compares an expression against a list of values using `eql`. The last branch is specified by `otherwise`. Hence, its canonical form is:

```lisp
(case <expression>
  ((<value>) <expression> <...>)
  ((<value>) <expression> <...>)
  (otherwise <expression> <...>))
```

A real-life example may be a function that uses the above mentioned calculation of *fizz buzz*:

```lisp
(defun fizz-buzz (n)
  (princ (case (is-fizz-buzz n)
               ((fizz) "Fizz!")
               ((buzz) "Buzz!")
               ((fizz-buzz) "Fizz-Buzz!")
               (otherwise n))))

(fizz-buzz 15)
;; => "Fizz-Buzz!"
```

Please note that `case` checks against a *list* of values, even in a single branch. Hence you must wrap values in parentheses. Additionally, do not quote values, as otherwise `'(foo)` is resolved to `(quote foo)`.

## Working with the console

### Writing output

To write an expression to the console use `print`. This will write the expression as seen by Lisp, e.g. including the double quotes when writing a string. Additionally, the output is terminated by a `#\newline` character:

```lisp
(print "Hello world!")
;; => "Hello world!"
```

If you want to omit the line break use `prin1` instead of `print`:

```lisp
(prin1 "Hello world!")
;; => "Hello world!"
```

To write the output in a more human-friendly way use `princ`. This also omits the line break, but will also omit the double quotes around strings:

```lisp
(princ "Hello world!")
;; => Hello world!
```

### Writing a line break

To write a line break to the console use `fresh-line`. This will produce a line break if the current write position is not at the beginning of the line, otherwise it won't do anything:

```lisp
(fresh-line)
```

### Writing to a string

If you don't want to write to the console, but to a string instead, use `prin1-to-string` or `princ-to-string`:

```lisp
(princ-to-string 'foobar)
;; => "foobar"
```

The common base for `prin1-to-string` and `princ-to-string` is `write-to-string`. They only differ in the presets used.

### Reading input

To read data from the console use `read`. This will interpret everything as Lisp data, hence you need to surround strings by double quotes, e.g.:

```lisp
(eval (read))
```

If you actually want the user to only enter a string use `read-line` instead, and you will always get a string:

```lisp
(eval (read-line))
```

Of course, as in any other language, running `eval` on basically arbitrary code is a major security concern, so you should be careful with this one.

### Reading input from a string

If you have Lisp code within a string that you want to turn into actual code use `read-from-string`:

```lisp
(eval (read-from-string "(princ \"Hello world!\")"))
```

## Working with files

### Opening a file

To open a file use `with-open-file` and provide the name of a stream, the name of the file and, optionally, some parameters to control how the file is being opened. Inside the body of `with-open-file` you can then use the stream's name as variable to write to:

```lisp
(with-open-file (my-stream "foo.txt" :direction :output :if-exists :supersede)
  (princ "Hello world!" my-stream))
```

If you specify `*standard-output*` as stream name, any writes to the standard output become redirected to the file.
