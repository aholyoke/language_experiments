
# Y := λg.(λx.g (x x)) (λx.g (x x))
Y = (lambda g: (lambda x: g(x(x)))(lambda x: g(x(x))))

# λr. λn.(1, if n = 0; else n × (r (n−1)))
G = (lambda r: (lambda n: 1 if n == 0 else n * (r(n - 1))))

# Z = λf.(λx.f (λv.((x x) v))) (λx.f (λv.((x x) v)))
Z = (lambda f: (lambda x: f(lambda v: x(x)(v)))(lambda x: f(lambda v: x(x)(v))))

# Z(G) == factorial
(lambda f: (lambda x: f(lambda v: x(x)(v)))(lambda x: f(lambda v: x(x)(v))))(lambda r: (lambda n: 1 if n == 0 else n * (r(n - 1))))



Z = (lambda f: (lambda x: f(lambda v: x(x)(v)))(lambda x: f(lambda v: x(x)(v))))((lambda q: (lambda r: (lambda n: q if n == 0 else n * (r(n - q)))))(1))


