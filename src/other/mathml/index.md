---
title: MathML Experiments
---

<script setup>
import DivisionLimits from './DivisionLimits.vue'
import NotANumber from './NotANumber.vue'
import YCombinator from './YCombinator.vue'
import ZeroOverZero from './ZeroOverZero.vue'
</script>

<!-- Copyright &copy; Kirk Rader  2023-2026 -->

# {{ $frontmatter.title }}

Unfortunately, VitePress only supports mathematical markup using an out-of-date
version of `markdown-it-mathjax3` with security issues. This page contains some
experiments with using native browser support for `MathML` directly, without any
intermediate markup.

::: details Y Combinator

As famously demonstrated by Haskell Curry, fixed-point combinators like _Y_,
defined below, allow self- and mutual recursion in the "pure" &lambda;-calculus
without recourse to an external environment in which otherwise-free variables
can be used to give "global" names to functions outside of the &lambda;
expressions which invoke them:

<YCombinator />

(Note that even though, for clarity, the preceding expression refers to the free
varialbe _g_ and declare a global name, _Y_, neither _g_ nor _Y_ appear to the
right of the equal sign in _Y_'s definition. The whole point of the preceding is
that a global environment is not actually necessary in order to define functions
that call themselves, nor collections of functions which call one another.)

While an important insight in the history of computational logic, this is
actually a very common pattern in Lisp programming or any code which takes
advantage of the functional programming paradigm. Noting that `let` is syntactic
sugar for `lambda`, the following implementation of _10!_ in Scheme is a
charmingly concise example of such a combinator:

```scheme
(let factorial ((a 1)
                (n 10))
    (if (<= n 1)
        a
        (factorial (* a n) (- n 1))))
```

Since the preceding implementation of the factorial function is properly
tail-recursive, the only limitation on the size of the initial value of `n` is
the amount of memory necessary to store the intermediate and final results of
the calls to `(* a n)` (which will quickly overlow to `bignum` values for even
relatively modest values of `n`).

:::

::: details IEEE 754 Division and `NaN`

The [IEEE 754] floating-point standard defines distinct values for +0 and -0 along
with +&infin; and -&infin;.

These enable consistent rules for sign agreement and division while minimizing
the need to treat 0 as a special case.

In particular, division by &pm;0 is well defined (in most cases) with an
infinite quotient since:

<DivisionLimits />

(the latter being simply the algebraic inverse of the former).

Unfortunately, the rules for division by &pm;0 do still have one irreconcilable
inconsistency.

- On the one hand, by the axioms of arithmetic, dividing any number by itself
  ought to result in 1.
- On the other hand, as shown above, dividing any number by &pm;0 ought to
  result in &pm;&infin;.

Since the result of <ZeroOverZero/> cannot be both &pm;1 and &pm;&infin; the
IEEE specification also defines a special value, usually displayed as `NaN`,
denoting "Not a Number," which is the value returned by such "impossible"
calculations:

<NotANumber />

`NaN` is fraught with its own complications. Conceptually, it is a well-defined
constant of a floating-point (i.e. numeric) type that is not actually a number
(as implied by the very term, `NaN`). The resulting rules for the behavior of
this "number that isn't really a number" make it somewhat challenging to deal
with in most real-world mathematical libraries and programming languages that
implement the IEEE specification. Many such systems opt, instead, to generate
operating system traps or throw exceptions in contexts where they would
otherwise be required to return `NaN`. Such behavior is allowed for in the IEEE
specification.

:::

[IEEE 754]: https://en.wikipedia.org/wiki/IEEE_754