#ifndef __CRZ_LISP_HH__
#define __CRZ_LISP_HH__

#include <type_traits>
#include <sstream>

namespace crz::tmp::lisp {

namespace __detail {

struct lang {};

template<typename T, typename = void>
constexpr bool is_identifier = true;
template<typename T>
constexpr bool
    is_identifier<T, std::void_t<decltype(T{}), std::enable_if_t<std::is_base_of_v<lang, T>>>> = false;
template<typename ...>
constexpr bool bad_pattern = false;

/*
 * Here are the definitions of the basic elements of this language:
 *
 * lang
 *   keyword
 *   value
 *     pair_value
 *       pair
 *     atom_value
 *       null_atom
 *         null
 *       number_atom
 *         number
 *       boolean_atom
 *         boolean
 *       procedure_atom
 *         closure
 *         primitive
 */

// keyword
struct keyword : public lang {};

struct lambda : public keyword {};
struct iff : public keyword {};
struct let : public keyword {};
struct letrec : public keyword {};
struct quote : public keyword {};
struct andd : public keyword {};
struct orr : public keyword {};
struct cond : public keyword {};
struct elsee : public keyword {};
struct dot : public keyword {};

// value
struct value : public lang {};

// atom
struct atom_value : public value {};

// pair
struct pair_value : public value {};
template<typename Head, typename Tail>
struct $ : public pair_value {};

struct null_atom : public atom_value {};
template<typename ...Ts>
struct __ : public null_atom {
  using type = __<Ts...>;
};
template<typename ...Ts>
using _ = typename __<Ts...>::type;
template<typename T, typename ...Ts>
struct __<T, Ts...> {
  using type = $<T, _<Ts...>>;
};
template<typename T, typename L>
struct __<T, dot, L> {
  using type = $<T, L>;
};
using null = _<>;

template<template<typename> typename Fun, typename List>
struct map_ {
  using type = null;
};
template<template<typename> typename Fun, typename List>
using map = typename map_<Fun, List>::type;
template<template<typename> typename Fun, typename Head, typename Tail>
struct map_<Fun, $<Head, Tail>> {
  using type = $<Fun<Head>, map<Fun, Tail>>;
};

template<template<typename> typename Fun, typename Value>
struct tree_map_ {
  using type = Fun<Value>;
};
template<template<typename> typename Fun, typename Value>
using tree_map = typename tree_map_<Fun, Value>::type;
template<template<typename> typename Fun, typename Head, typename Tail>
struct tree_map_<Fun, $<Head, Tail>> {
  using type = $<Fun<Head>, Fun<Tail>>;
};

// other atom
struct number_atom : public atom_value {};
template<long long N>
struct number : public number_atom {};

struct boolean_atom : public atom_value {};
template<bool B>
struct boolean : public boolean_atom {};

struct procedure_atom : public atom_value {};
template<typename Params, typename Body, typename Env>
struct closure : public procedure_atom {};
template<template<typename Args> typename Fun>
struct primitive : public procedure_atom {};

template<typename T>
constexpr bool is_false = false;
template<>
constexpr bool is_false<boolean<false>> = true;
template<typename T>
constexpr bool is_true = !is_false<T>;


/*
 * Here are the definitions of the evaluation environment
 * and the variable-lookup function
 */

// environment
struct environment {};
struct empty_env : public environment {};
// simple environment for let/apply
template<typename Var, typename Value, typename SavedEnv>
struct normal_env : public environment {
  static_assert(std::is_base_of_v<environment, SavedEnv>);
};
// recursive environment for letrec
template<typename Var, typename Value, typename SavedEnv, bool More>
struct recurs_env : public environment {
  static_assert(std::is_base_of_v<environment, SavedEnv>);
};

template<typename Env, typename SrcEnv, typename RecEnv>
struct rebuild_env_aux_ {
  using type = Env;
};
template<typename CurEnv, typename SrcEnv, typename RecEnv>
struct rebuild_env_ {
  using type = typename rebuild_env_aux_<CurEnv, SrcEnv, RecEnv>::type;
};
template<typename CurEnv, typename SrcEnv, typename RecEnv>
using rebuild_env = typename rebuild_env_<CurEnv, SrcEnv, RecEnv>::type;
template<typename Var, typename Value, typename SavedEnv, typename SrcEnv, typename RecEnv>
struct rebuild_env_aux_<normal_env<Var, Value, SavedEnv>, SrcEnv, RecEnv> {
  using type = normal_env<Var, Value, rebuild_env<SavedEnv, SrcEnv, RecEnv>>;
};
template<typename Var, typename Value, typename SavedEnv, bool More, typename SrcEnv, typename RecEnv>
struct rebuild_env_aux_<recurs_env<Var, Value, SavedEnv, More>, SrcEnv, RecEnv> {
  using type = recurs_env<Var, Value, rebuild_env<SavedEnv, SrcEnv, RecEnv>, More>;
};
template<typename SrcEnv, typename RecEnv>
struct rebuild_env_<SrcEnv, SrcEnv, RecEnv> {
  using type = RecEnv;
};

// variable-lookup
template<typename RecEnv, typename Var, typename Env>
struct lookup_var_;
template<typename Var, typename Env>
using lookup_var = typename lookup_var_<Env, Var, Env>::type;
template<typename RecEnv, typename Var,
    typename Value, typename SavedEnv>
struct lookup_var_<RecEnv, Var, normal_env<Var, Value, SavedEnv>> {
  using type = Value;
};
template<typename RecEnv, typename Var,
    typename Var1, typename Value, typename SavedEnv>
struct lookup_var_<RecEnv, Var, normal_env<Var1, Value, SavedEnv>> {
  using type = typename lookup_var_<SavedEnv, Var, SavedEnv>::type;
};
template<typename RecEnv, typename Var,
    typename Value, typename SavedEnv, bool More>
struct lookup_var_<RecEnv, Var, recurs_env<Var, Value, SavedEnv, More>> {
  template<typename Value1>
  struct trans_ {
    using type = Value1;
  };
  template<typename Params, typename Body, typename Env>
  struct trans_<closure<Params, Body, Env>> {
    using type = closure<Params, Body, rebuild_env<Env, SavedEnv, RecEnv>>;
  };
  template<typename Value1>
  using trans = typename trans_<Value1>::type;

  using type = tree_map<trans, Value>;
};
template<typename RecEnv, typename Var,
    typename Var1, typename Value, typename SavedEnv, bool More>
struct lookup_var_<RecEnv, Var, recurs_env<Var1, Value, SavedEnv, More>> {
  using type = typename lookup_var_<std::conditional_t<More, RecEnv, SavedEnv>, Var, SavedEnv>::type;
};


/*
 * Here is the definition of the expression-evaluation (eval&apply loop)
 */

// eval
template<typename Expr, typename Env, typename = void>
struct eval_expr_ {
  static_assert(bad_pattern<Expr>, "bad expression");
};
template<typename Expr, typename Env>
using eval_expr = typename eval_expr_<Expr, Env>::type;

// eval variable
template<typename Var, typename Env>
struct eval_expr_<Var, Env, std::enable_if_t<is_identifier<Var>>> {
  using type = lookup_var<Var, Env>;
};

// eval atom
template<typename Atom, typename Env>
struct eval_expr_<Atom, Env, std::void_t<std::enable_if_t<!is_identifier<Atom>>,
    std::enable_if_t<std::is_base_of_v<atom_value, Atom>>>> {
  using type = Atom;
};

// eval quote
template<typename Expr, typename Env>
struct eval_expr_<$<quote, $<Expr, null>>, Env> {
  using type = Expr;
};

// eval lambda
template<typename Params, typename Body, typename Env>
struct eval_expr_<$<lambda, $<Params, $<Body, null>>>, Env> {
  using type = closure<Params, Body, Env>;
};

// eval if
template<typename Pred, typename Then, typename Else, typename Env>
struct eval_expr_<$<iff, $<Pred, $<Then, $<Else, null>>>>, Env> {
  using type = eval_expr<std::conditional_t<is_true<eval_expr<Pred, Env>>, Then, Else>, Env>;
};

// eval cond
template<typename Clauses, typename Env>
struct eval_cond_ {
  static_assert(bad_pattern<Clauses>, "bad clauses");
};
template<typename Clauses, typename Env>
using eval_cond = typename eval_cond_<Clauses, Env>::type;

template<typename Pred, typename Expr, typename RestClauses, typename Env>
struct eval_cond_<$<$<Pred, $<Expr, null>>, RestClauses>, Env> {
  static auto infer() {
    using pred_value = eval_expr<Pred, Env>;
    if constexpr (is_true<pred_value>) {
      return eval_expr<Expr, Env>{};
    } else {
      return eval_cond<RestClauses, Env>{};
    }
  }

  using type = decltype(infer());
};

template<typename Expr, typename RestClauses, typename Env>
struct eval_cond_<$<$<elsee, $<Expr, null>>, RestClauses>, Env> {
  using type = eval_expr<Expr, Env>;
};
template<typename Env>
struct eval_cond_<null, Env> {
  using type = null; // default
};
template<typename Clauses, typename Env>
struct eval_expr_<$<cond, Clauses>, Env> {
  using type = eval_cond<Clauses, Env>;
};

// eval let
template<typename Bindings, typename EvalEnv, typename SavedEnv>
struct extend_let_env_ {
  static_assert(bad_pattern<Bindings>, "bad bindings pattern");
};
template<typename Var, typename Expr, typename RestBindings, typename EvalEnv, typename SavedEnv>
struct extend_let_env_<$<$<Var, $<Expr, null>>, RestBindings>, EvalEnv, SavedEnv> {
  using type = typename extend_let_env_<RestBindings, EvalEnv,
      normal_env<Var, eval_expr<Expr, EvalEnv>, SavedEnv>>::type;
};
template<typename EvalEnv, typename SavedEnv>
struct extend_let_env_<null, EvalEnv, SavedEnv> {
  using type = SavedEnv;
};
template<typename Bindings, typename SavedEnv>
using extend_let_env = typename extend_let_env_<Bindings, SavedEnv, SavedEnv>::type;
template<typename Bindings, typename Body, typename Env>
struct eval_expr_<$<let, $<Bindings, $<Body, null>>>, Env> {
  using type = eval_expr<Body, extend_let_env<Bindings, Env>>;
};

// eval letrec
template<typename Bindings, typename SavedEnv, bool More>
struct extend_letrec_env_ {
  static_assert(bad_pattern<Bindings>, "bad bindings pattern");
};
template<typename Var, typename Expr, typename RestBindings, typename SavedEnv, bool More>
struct extend_letrec_env_<$<$<Var, $<Expr, null>>, RestBindings>, SavedEnv, More> {
  using type = typename extend_letrec_env_<RestBindings,
      recurs_env<Var, eval_expr<Expr, SavedEnv>, SavedEnv, More>, true>::type;
};
template<typename SavedEnv, bool More>
struct extend_letrec_env_<null, SavedEnv, More> {
  using type = SavedEnv;
};
template<typename Bindings, typename SavedEnv>
using extend_letrec_env = typename extend_letrec_env_<Bindings, SavedEnv, false>::type;
template<typename Bindings, typename Body, typename Env>
struct eval_expr_<$<letrec, $<Bindings, $<Body, null>>>, Env> {
  using type = eval_expr<Body, extend_letrec_env<Bindings, Env>>;
};

// eval and
template<typename Preds, typename Env>
struct eval_and_ {
  static_assert(bad_pattern<Preds>, "bad predicates");
};
template<typename Preds, typename Env>
using eval_and = typename eval_and_<Preds, Env>::type;

template<typename Pred, typename RestPreds, typename Env>
struct eval_and_<$<Pred, RestPreds>, Env> {
  static auto infer() {
    using pred_value = eval_expr<Pred, Env>;
    if constexpr (is_true<pred_value>) {
      return eval_and<RestPreds, Env>{};
    } else {
      return pred_value{};
    }
  }

  using type = decltype(infer());
};

template<typename Env>
struct eval_and_<null, Env> {
  using type = boolean<true>;
};
template<typename Preds, typename Env>
struct eval_expr_<$<andd, Preds>, Env> {
  using type = eval_and<Preds, Env>;
};

// eval or
template<typename Preds, typename Env>
struct eval_or_ {
  static_assert(bad_pattern<Preds>, "bad predicates");
};
template<typename Preds, typename Env>
using eval_or = typename eval_or_<Preds, Env>::type;

template<typename Pred, typename RestPreds, typename Env>
struct eval_or_<$<Pred, RestPreds>, Env> {
  static auto infer() {
    using pred_value = eval_expr<Pred, Env>;
    if constexpr (is_true<pred_value>) {
      return pred_value{};
    } else {
      return eval_or<RestPreds, Env>{};
    }
  }

  using type = decltype(infer());
};

template<typename Env>
struct eval_or_<null, Env> {
  using type = boolean<false>;
};
template<typename Preds, typename Env>
struct eval_expr_<$<orr, Preds>, Env> {
  using type = eval_or<Preds, Env>;
};

// eval apply
template<typename Params, typename Args, typename SavedEnv, typename = void>
struct extend_apply_env_ {
  static_assert(bad_pattern<Params, Args>, "bad arguments");
};
template<typename Params, typename Args, typename SavedEnv>
using extend_apply_env = typename extend_apply_env_<Params, Args, SavedEnv>::type;
template<typename Param, typename RestParams, typename Arg, typename RestArgs, typename SavedEnv>
struct extend_apply_env_<$<Param, RestParams>, $<Arg, RestArgs>, SavedEnv> {
  using type = extend_apply_env<RestParams, RestArgs, normal_env<Param, Arg, SavedEnv>>;
};
template<typename Param, typename Args, typename SavedEnv>
struct extend_apply_env_<Param, Args, SavedEnv, std::enable_if_t<is_identifier<Param>>> {
  using type = normal_env<Param, Args, SavedEnv>;
};
template<typename SavedEnv>
struct extend_apply_env_<null, null, SavedEnv> {
  using type = SavedEnv;
};
template<typename Proc, typename Args>
struct apply_proc_ {
  static_assert(bad_pattern<Proc>, "bad procedure");
};
template<typename Proc, typename Args>
using apply_proc = typename apply_proc_<Proc, Args>::type;
template<typename Params, typename Body, typename Env, typename Args>
struct apply_proc_<closure<Params, Body, Env>, Args> {
  using type = eval_expr<Body, extend_apply_env<Params, Args, Env>>;
};
template<template<typename> typename Fun, typename Args>
struct apply_proc_<primitive<Fun>, Args> {
  using type = Fun<Args>;
};
template<typename Rator, typename Rand, typename Env>
struct eval_expr_<$<Rator, Rand>, Env> {
  template<typename Arg>
  using eval_arg = eval_expr<Arg, Env>;
  using type = apply_proc<eval_expr<Rator, Env>, map<eval_arg, Rand>>;
};

/*
 * Here is the definition of the wrapper for some runtime representation
 */
template<typename T>
struct runtime;

template<long long N>
struct runtime<number<N>> {
  static constexpr long long value = N;

  static std::ostream &output(std::ostream &os, bool = true) {
    return os << N;
  }
};

template<bool B>
struct runtime<boolean<B>> {
  static constexpr bool value = B;

  static std::ostream &output(std::ostream &os, bool = true) {
    return os << (B ? "#t" : "#f");
  }
};

template<>
struct runtime<null> {
  static std::ostream &output(std::ostream &os, bool = true) {
    return os << "()";
  }
};

template<typename Head, typename Tail>
struct runtime<$<Head, Tail>> {
  static std::ostream &output(std::ostream &os, bool list_head = true) {
    if (list_head)
      os << "(";
    runtime<Head>::output(os);
    if constexpr (std::is_base_of_v<pair_value, Tail>) {
      runtime<Tail>::output(os << " ", false);
    } else if constexpr (!std::is_same_v<Tail, null>) {
      runtime<Tail>::output(os << " . ");
    }
    if (list_head)
      os << ")";
    return os;
  }
};


/*
 * Here are the definitions of some primitive procedures:
 *
 * list operation:
 *      cons, car, cdr,
 * number comparison:
 *      is_lt, is_gt, is_le, is_ge
 * equivalence check:
 *      is_eq
 * type predicate:
 *      is_number, is_boolean, is_procedure, is_pair, is_null, is_atom, is_symbol,
 * arithmetic operation:
 *      add, sub, mul, div, rem,
 * others:
 *      apply
 */
namespace prim {
#define PRIM(name)\
template<typename Args, typename = void>\
struct name ## _ { static_assert(bad_pattern<Args>, "bad arguments"); };\
template<typename Args>\
using name = typename name ## _<Args>::type;


// some primitives for list manipulation
PRIM(cons)
template<typename Head, typename Tail>
struct cons_<$<Head, $<Tail, null>>> {
  using type = $<Head, Tail>;
};
PRIM(car)
template<typename Head, typename Tail>
struct car_<$<$<Head, Tail>, null>> {
  using type = Head;
};
PRIM(cdr)
template<typename Head, typename Tail>
struct cdr_<$<$<Head, Tail>, null>> {
  using type = Tail;
};


// some primitives for comparison
#define COMPARE(name, opt)\
PRIM(name)\
template<long long N1, long long N2>\
struct name ## _<$<number<N1>, $<number<N2>, null>>> { using type = boolean<(N1 opt N2)>; };
// <
COMPARE(is_lt, <)
// >
COMPARE(is_gt, >)
// <=
COMPARE(is_le, <=)
// >=
COMPARE(is_ge, >=)
#undef COMPARE


// equivalence check, i.e., eq?/equal?/eqv? in r5rs
PRIM(is_eq)
template<typename T1, typename T2>
struct is_eq_<$<T1, $<T2, null>>> {
  using type = boolean<std::is_same_v<T1, T2>>;
};


// some primitives for type predicate
#define PRED(name, base)\
template<typename Args, typename = void>\
struct is_ ## name ## _;\
template<typename Args>\
using is_ ## name = typename is_ ## name ## _<Args>::type;\
template<typename T>\
struct is_## name ## _<$<T, null>, std::enable_if_t<!is_identifier<T>>> { using type = boolean<std::is_base_of_v<base, T>>; };\
template <typename T, typename>\
struct is_ ## name ## _ { using type = boolean<false>; };
// number?
PRED(number, number_atom)
// boolean?
PRED(boolean, boolean_atom)
// procedure?
PRED(procedure, procedure_atom)
// pair?
PRED(pair, pair_value)
// null?
PRED(null, null_atom)
// atom?
PRED(atom, atom_value)
// symbol?
PRIM(is_symbol)

template<typename T>
struct is_symbol_<$<T, null>> {
  static auto infer() {
    if constexpr (is_identifier<T>) {
      return boolean<true>{};
    } else {
      return boolean<!std::is_base_of_v<value, T>>{};
    }
  }

  using type = decltype(infer());
};

#undef PRED


// some arithmetic primitives which support variant arguments, i.e., add/sub/mul/div
#define ADD_STYLE(name, init, opt)\
template<long long Acc, typename Args>\
struct name ## _ { static_assert(bad_pattern<Args>, "bad arguments"); };\
template<typename Args>\
using name = typename name ## _<init, Args>::type;\
template<long long Acc, long long N, typename RestArgs>\
struct name ## _<Acc, $<number<N>, RestArgs>> { using type = typename name ## _<Acc opt N, RestArgs>::type; };\
template<long long Acc>\
struct name ## _<Acc, null> { using type = number<Acc>; };
ADD_STYLE(add, 0, +)
ADD_STYLE(mul, 1, *)
#undef ADD_STYLE
#define SUB_STYLE(name, init, opt)\
template<long long Acc, typename Args, bool First>\
struct name ## _ { static_assert(bad_pattern<Args>, "bad arguments"); };\
template<typename Args>\
using name = typename name ## _<init, Args, true>::type;\
template<long long Acc, long long N, typename RestArgs>\
struct name ## _<Acc, $<number<N>, RestArgs>, true> { using type = typename name ## _<N, RestArgs, false>::type; };\
template<long long Acc, long long N, typename RestArgs>\
struct name ## _<Acc, $<number<N>, RestArgs>, false> { using type = typename name ## _<Acc opt N, RestArgs, false>::type; };\
template<long long Acc, long long N>\
struct name ## _<Acc, $<number<N>, null>, true> { using type = number<Acc opt N>; };\
template<long long Acc, long long N>\
struct name ## _<Acc, $<number<N>, null>, false> { using type = number<Acc opt N>; };
SUB_STYLE(sub, 0, -)
SUB_STYLE(div, 1, /)
#undef SUB_STYLE


// remainder
PRIM(rem)
template<long long N1, long long N2>
struct rem_<$<number<N1>, $<number<N2>, null>>> {
  using type = number<N1 % N2>;
};


// apply
PRIM(apply)
template<typename Proc, typename Args>
struct apply_<$<Proc, $<Args, null>>> {
  using type = apply_proc<Proc, Args>;
};
#undef PRIM
}

template<typename SavedEnv, typename ...Prims>
struct build_env_ {
  using type = SavedEnv;
};
template<typename SavedEnv, typename Name, typename Prim, typename ...RestPrims>
struct build_env_<SavedEnv, $<Name, Prim>, RestPrims...> {
  using type = typename build_env_<normal_env<Name, Prim, SavedEnv>, RestPrims...>::type;
};
template<typename SavedEnv, typename ...Prims>
using build_env = typename build_env_<SavedEnv, Prims...>::type;
}

// environment
#define PRIM(name) __detail::$<struct name, __detail::primitive<__detail::prim::name>>
using prim_env = __detail::build_env<
    __detail::empty_env,

    PRIM(apply),
    PRIM(add), PRIM(sub), PRIM(mul), PRIM(div),
    PRIM(is_eq), PRIM(is_lt), PRIM(is_gt), PRIM(is_le), PRIM(is_ge),
    PRIM(is_number), PRIM(is_boolean), PRIM(is_procedure), PRIM(is_pair), PRIM(is_null),
    PRIM(is_symbol), PRIM(is_atom),
    PRIM(cons), PRIM(car), PRIM(cdr)
>;
#undef PRIM

#define USE(name) using name = __detail::name
USE(lambda);
USE(iff);
USE(let);
USE(letrec);
USE(quote);
USE(andd);
USE(orr);
USE(null);
USE(cond);
USE(elsee);
USE(dot);
#undef USE

#define V(x) struct x
#define N(n) __detail::number<n>
#define B(b) __detail::boolean<b>
template<typename ...Ts>
using _ = __detail::_<Ts...>;


/*
 * Here is the definitions of some library procedures:
 *
 * list, nott, exist, forall, map, flat_map, filter, foldl, foldr, append,
 * partial, zip, interleave, length, memq, assq, reverse, list_ref, list_tail
 */
namespace lib {

template<typename Expr>
using eval = __detail::eval_expr<Expr, prim_env>;

using list = eval<
    _<lambda, V(lst), lst>
>;

using nott = eval<
    _<lambda, V(x),
        _<iff, x,
            B(false),
            B(true)>>
>;

using exist = eval<
    _<lambda, _<V(pred), V(lst)>,
        _<letrec, _<_<V(iter), _<lambda, _<V(lst)>,
            _<cond,
                _<_<is_null, lst>, B(false) >,
                _<_<pred, _<car, lst>>, B(true) >,
                _<elsee, _<iter, _<cdr, lst>>>>>>>,
            _<iter, lst>>>
>;

using forall = eval<
    _<lambda, _<V(pred), V(lst)>,
        _<letrec, _<_<V(iter), _<lambda, _<V(lst)>,
            _<cond,
                _<_<is_null, lst>, B(true) >,
                _<_<pred, _<car, lst>>, _<iter, _<cdr, lst>>>,
                _<elsee, B(false)>>>>>,
            _<iter, lst>>>
>;

using map = eval<
    _<letrec, _<_<V(single_map), _<lambda, _<V(fun), V(lst)>,
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
        _<lambda, _<V(fun), dot, V(lsts)>,
            _<multi_map, fun, lsts>>>
>;

using foldl = eval<
    _<lambda, _<V(fun), V(init), V(lst)>,
        _<letrec, _<_<V(iter), _<lambda, _<V(acc), V(lst)>,
            _<iff, _<is_null, lst>,
                acc,
                _<iter, _<fun, acc, _<car, lst>>, _<cdr, lst>>>>>>,
            _<iter, init, lst>>>
>;

using foldr = eval<
    _<lambda, _<V(fun), V(init), V(lst)>,
        _<letrec, _<_<V(recs), _<lambda, _<V(lst)>,
            _<iff, _<is_null, lst>,
                init,
                _<fun, _<car, lst>, _<recs, _<cdr, lst>>>>>>>,
            _<recs, lst>>>
>;

using length = eval<
    _<lambda, _<V(lst)>,
        _<foldl,
            _<lambda, _<V(acc), V(cur)>, _<add, acc, N(1)>>,
            N(0), lst>>
>;

using filter = eval<
    _<lambda, _<V(pred), V(lst)>,
        _<foldr,
            _<lambda, _<V(cur), V(acc)>,
                _<iff, _<pred, cur>,
                    _<cons, cur, acc>,
                    acc>>,
            null, lst>>
>;

using append = eval<
    _<lambda, V(lsts),
        _<foldr, _<lambda, _<V(l1), V(l2)>, _<foldr, cons, l2, l1>>, null, lsts>>
>;

using partial = eval<
    _<lambda, _<V(fun), dot, V(cached_args)>,
        _<lambda, V(args),
            _<apply, fun, _<append, cached_args, args>>>>
>;

using zip = eval<
    _<partial, map, list>
>;

using flat_map = eval<
    _<lambda, V(args),
        _<apply, append, _<apply, map, args>>>
>;

using interleave = eval<
    _<partial, flat_map, list>
>;

using reverse = eval<
    _<partial, foldl, _<lambda, _<V(acc), V(cur)>, _<cons, cur, acc>>, null>
>;

using memq = eval<
    _<lambda, _<V(x), V(lst)>,
        _<letrec, _<_<V(iter), _<lambda, _<V(lst)>,
            _<cond,
                _<_<is_null, lst>, B(false) >,
                _<_<is_eq, x, _<car, lst>>, lst>,
                _<elsee, _<iter, _<cdr, lst>>>>>>>,
            _<iter, lst>>>
>;

using assq = eval<
    _<lambda, _<V(x), V(lst)>,
        _<letrec, _<_<V(iter), _<lambda, _<V(lst)>,
            _<iff, _<is_null, lst>,
                B(false),
                _<let,
                    _<
                        _<V(head), _<car, lst>>,
                        _<V(tail), _<cdr, lst>>>,
                    _<iff, _<is_eq, x, _<car, head>>,
                        head,
                        _<iter, tail>>>>>>>,
            _<iter, lst>>>
>;

using list_ref = eval<
    _<letrec, _<_<V(iter), _<lambda, _<V(lst), V(k)>,
        _<iff, _<is_eq, k, N(0) >,
            _<car, lst>,
            _<iter, _<cdr, lst>, _<sub, k, N(1)>>>>>>,
        iter>
>;

using list_tail = eval<
    _<letrec, _<_<V(iter), _<lambda, _<V(lst), V(k)>,
        _<iff, _<is_eq, k, N(0) >,
            lst,
            _<iter, _<cdr, lst>, _<sub, k, N(1)>>>>>>,
        iter>
>;

}

#define LIB(name) __detail::$<struct name, lib::name>

using lib_env = __detail::build_env<
    prim_env,

    LIB(list), LIB(nott), LIB(map), LIB(zip), LIB(foldl), LIB(foldr), LIB(length), LIB(filter),
    LIB(append), LIB(exist), LIB(forall), LIB(reverse), LIB(memq), LIB(assq),
    LIB(list_ref), LIB(list_tail), LIB(flat_map), LIB(partial), LIB(interleave)
>;

template<typename Expr>
using eval = __detail::eval_expr<Expr, lib_env>;

template<typename T>
using runtime = __detail::runtime<T>;

}

#endif //__CRZ_LISP_HH__
