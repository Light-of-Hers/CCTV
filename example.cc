#include <iostream>
#include "crz_lisp.hh"

template<typename T1, typename T2>
void assert_eq() {
  static_assert(std::is_same_v<T1, T2>);
}

int main() {
  using namespace crz::tmp::lisp;
  {
    /*
     * (define fib
     *   (lambda (n)
     *     (letrec
     *       (
     *         (iter (lambda (n a b)
     *           (if (eqv? n 0)
     *             a
     *             (iter (- n 1) b (+ a b))))))
     *       (iter n 0 1))))
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
    assert_eq<expr, N(55) >();
    runtime<expr>::output(std::cout) << std::endl; // 55
  }
  {
    // (map + (list 1 2) (list 3 4) (list 5 6))
    using expr = eval<
        _<map, add, _<list, N(1), N(2) >, _<list, N(3), N(4) >, _<list, N(5), N(6)>>
    >;
    assert_eq<expr, _<N(9), N(12)>>();
    runtime<expr>::output(std::cout) << std::endl; // (9 12)
  }
  {
    // (reverse '(1 2 #f #t))
    using expr = eval<
        _<reverse, _<quote, _<N(1), N(2), B(false), B(true)>>>
    >;
    assert_eq<expr, _<B(true), B(false), N(2), N(1)>>();
    runtime<expr>::output(std::cout) << std::endl; // (#t #f 2 1)
  }
  {
    // (interleave (list 1 2) (list 3 4))
    using expr = eval<
        _<interleave, _<list, N(1), N(2) >, _<list, N(3), N(4)>>
    >;
    assert_eq<expr, _<N(1), N(3), N(2), N(4)>>();
    runtime<expr>::output(std::cout) << std::endl; // (1 3 2 4)
  }
  {
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
    assert_eq<expr, B(true) >();
    runtime<expr>::output(std::cout) << std::endl; // #t
  }
  {
    /*
     * (letrec
     *   ((fs (cons
     *          (lambda (n)
     *            (if (eqv? n 0)
     *                #t
     *                ((cdr fs) (- n 1))))
     *          (lambda (n)
     *            (if (eqv? n 0)
     *                #f
     *                ((car fs) (- n 1)))))))
     *   ((car fs) 12))
     */
    using expr = eval<
        _<letrec,
            _<_<V(fs), _<cons,
                _<lambda, _<V(n)>,
                    _<iff, _<is_eq, n, N(0) >,
                        B(true),
                        _<_<cdr, fs>, _<sub, n, N(1)>>>>,
                _<lambda, _<V(n)>,
                    _<iff, _<is_eq, n, N(0) >,
                        B(false),
                        _<_<car, fs>, _<sub, n, N(1)>>>>>>>,
            _<_<car, fs>, N(12)>>
    >;
    assert_eq<expr, B(true) >();
    runtime<expr>::output(std::cout) << std::endl; // #t
  }
}