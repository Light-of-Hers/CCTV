# CCTV: <u>C</u>++ <u>C</u>ompile-<u>T</u>ime e<u>V</u>aluator for scheme

[TOC]

A scheme dialect based on C++ template meta programming.

Evaluation is done in compile-time.

Requires C++17 standard.



## Introduction

The pure functional core of the scheme metacircular evaluator turns out to be fairly easy to implement entirely in C++ template metaprogramming (TMP). This repo implements a simple scheme dialect called "CCTV-scheme" based on C++ TMP, by which you can write scheme code using C++ template like:

```c++
_<lambda, _<V(pred), V(lst)>,
    _<letrec,
        _<
            _<V(iter), _<lambda, _<V(lst)>,
                _<cond,
                    _<_<is_null, lst>, B(false) >,
                    _<_<pred, _<car, lst>>, B(true) >,
                    _<elsee, _<iter, _<cdr, lst>>>>>>>,
        _<iter, lst>>>
```

which is equivalent to scheme code:

```scheme
(lambda (pred lst)
  (letrec
      (
       (iter (lambda (lst)
               (cond
                 ((null? lst) #f)
                 ((pred (car lst)) #t)
                 (else (iter (cdr lst)))))))
    (iter lst)))
```





## Example

### Fibonacci

```C++
using namespace crz::tmp::lisp;

/*
 *   (lambda (n)
 *     (letrec
 *       (
 *         (iter (lambda (n a b)
 *           (if (eqv? n 0)
 *             a
 *             (iter (- n 1) b (+ a b))))))
 *       (iter n 0 1)))
 */
using fib = eval<
    _<lambda, _<V(n)>,
        _<letrec,
            _<
                _<V(iter), _<lambda, _<V(n), V(a), V(b)>,
                    _<iff, _<is_eq, n, N(0) >,
                        a,
                        _<iter, _<sub, n, N(1) >, b, _<add, a, b>>>>>>,
            _<iter, n, N(0), N(1)>>>
>;
// (fib 10)
using expr = eval<_<fib, N(10)>>;
runtime<expr>::output(std::cout) << std::endl; // 55
```



### Map

```C++
using namespace crz::tmp::lisp;

// (map + (list 1 2) (list 3 4) (list 5 6))
using expr = eval<
    _<map, add, _<list, N(1), N(2) >, _<list, N(3), N(4) >, _<list, N(5), N(6)>>
>;
runtime<expr>::output(std::cout) << std::endl; // (9 12)
```



### Flat Map

```C++
using namespace crz::tmp::lisp;

// (flat-map list (list 1 2) (list 3 4))
using expr = eval<
    _<flat_map, list, _<list, N(1), N(2) >, _<list, N(3), N(4)>>
>; // interleave
runtime<expr>::output(std::cout) << std::endl; // (1 3 2 4)
```



### Mutual Recursion

```C++
using namespace crz::tmp::lisp;

/*
 * (letrec
 *   (
 *     (even? (lambda (n)
 *       (if (eqv? n zero)
 *         #t
 *         (odd? (sub1 n)))))
 *     (one 1)
 *     (odd? (lambda (n)
 *       (if (eqv? n zero)
 *         #f
 *         (even? (sub1 n)))))
 *     (sub1 (lambda (n) (- n one)))
 *     (zero (sub1 one)))
 *   (even? 12))
 */
using expr = eval<
    _<letrec,
        _<
            _<V(is_even), _<lambda, _<V(n)>,
                _<iff, _<is_eq, n, V(zero)>,
                    B(true),
                    _<V(is_odd), _<V(sub1), n>>>>>,
            _<V(one), N(1) >,
            _<V(is_odd), _<lambda, _<V(n)>,
                _<iff, _<is_eq, n, V(zero)>,
                    B(false),
                    _<is_even, _<V(sub1), n>>>>>,
            _<V(sub1), _<lambda, _<V(n)>, _<sub, n, one>>>,
            _<V(zero), _<sub1, one>>>,
        _<is_even, N(12)>>
>;
runtime<expr>::output(std::cout) << std::endl; // #t
```



Since the grammer of the CCTV-scheme is very like other scheme dialects, there is no need to show many examples.



## Usage

You can write CCTV-scheme just like other schemes. But there're some differences in code form that should be concentrated on:

+ Use `_<` and `>` instead of `(` and `)` respectively to represent list.

+ Use comma instead of whitespace to seperate identities.

+ Use macro `N(n)` and `B(b)` to represent number `n` and boolean `b` respectively.

+ Use macro `V(v)` to represent variable identity `v`. The macro just expands to `struct v`. It is used to reference a variable that has not appeared yet, and is usually used for parameter declaration. If the variable `v` has been declared already, then you can just use `v` instead of `V(v)`. For example:

  ```C++
  _<lambda, _<V(x)>, // first appearance of 'x'
  	x> // x has been declared already
  ```



## Features Supported

### Basic Data Type

+ integer number
+ boolean
+ pair/list/null



### Keyword

+ `lambda`
+ `let`
+ `letrec`: like the `letrec` in racket, that is, like the combination of `letrec` and `let*` in r5rs. 
+ `quote`
+ `null`
+ `iff`: represents `if` in scheme
+ `cond`
+ `elsee`: represents `else` in scheme
+ `andd`: represents `and` in scheme
+ `orr`: represents `or` in scheme



### Syntax

+ Support variant arguments. For example, the definition of procedure `list` in scheme:

  ```scheme
  (lambda lst lst)
  ```

  is equivalent to:

  ```C++
  _<lambda, V(lst), lst>
  ```

  But the scheme code below:

  ```scheme
  (lambda (head . tail) tail)
  ```

  cannot be transform to CCTV-scheme directly. Instead, you can use `let` binding to get each argument:

  ```C++
  _<lambda, V(args),
  	_<let, _<
  		_<V(head), _<car, args>>,
  		_<V(tail), _<cdr, args>>>,
  		tail>>
  ```

+ Cannot use `'` to abbreviate `quote`. Instead, you should use `quote` explicitly.



### Primitive Procedure

+ list operation: `cons`, `car`, `cdr`
+ number comparison: `is_lt`, `is_gt`, `is_le`, `is_ge`
+ equivalence check: `is_eq`
+ type predicate: `is_number`, `is_boolean`, `is_procedure`, `is_pair`, `is_null`, `is_atom`, `is_symbol`
+ arithmetic operation: `add`, `sub`, `mul`, `div`, `rem`
+ others: `apply`



### Library Procedure

The library procedures are all coded using CCTV-scheme. For example:

```C++
using map = eval<
    _<letrec,
        _<
            _<V(single_map), _<lambda, _<V(fun), V(lst)>,
                _<iff, _<is_null, lst>,
                    null,
                    _<cons,
                        _<fun, _<car, lst>>,
                        _<single_map, fun, _<cdr, lst>>>>>>,
            _<V(multi_map), _<lambda, _<V(fun), V(lsts)>,
                _<iff, _<exist, is_null, lsts>,
                    null,
                    _<cons,
                        _<apply, fun, _<single_map, car, lsts>>,
                        _<multi_map, fun, _<single_map, cdr, lsts>>>>>>>,
        _<lambda, V(args),
            _<let,
                _<
                    _<V(fun), _<car, args>>,
                    _<V(lsts), _<cdr, args>>>,
                _<multi_map, fun, lsts>>>>
>;
```

Here are the procedures already implemented as library:

```
list, nott, exist, forall, map, flat_map, filter, foldl, foldr, append,
partial, zip, interleave, length, memq, assq, reverse, list_ref, list_tail
```

