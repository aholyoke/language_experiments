# ~*~ encoding: utf-8 ~*~
# Implementation of recursive factorial using only lambdas
# There are no recursive calls yet we achieve recursion using fixed point combinators

# Y combinator
# Unfortunately this will not work with applicative order reduction (Python), so we will use Z combinator
# Y := λg.(λx.g (x x)) (λx.g (x x))
Y = (lambda g: (lambda x: g(x(x)))(lambda x: g(x(x))))

# Z combinator
# Like the Y combinator except it has an extra "thunking" step to prevent infinite reduction
# Z = λf.(λx.f (λv.x x v)) (λx.f (λv.x x v))
Z = (lambda f: (lambda x: f(lambda v: x(x)(v)))(lambda x: f(lambda v: x(x)(v))))

# The definition of factorial
# Takes a continuation r which will be the recursive definition of factorial
# λr. λn.(1, if n = 0; else n × (r (n−1)))
G = (lambda r: (lambda n: 1 if n == 0 else n * (r(n - 1))))

# Z(G) = factorial
# The definition of factorial G is passed to Z as argument f
# Since Z is a fixed point combinator it satisfies Z(G) = G(Z(G))
# G(Z(G)) tells us that parameter r of G is passed the recursive definition of factorial
factorial = (lambda f: (lambda x: f(lambda v: x(x)(v)))(lambda x: f(lambda v: x(x)(v))))(
    lambda r: (lambda n: 1 if n == 0 else n * (r(n - 1))))

# demonstration
print(factorial(5))
print(factorial(6))
