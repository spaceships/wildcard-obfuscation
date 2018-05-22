from random import randrange

n = 20
#  p = random_prime(2**(n+1), lbound=2**n)
#  p = 97
p = random_prime(2**129, lbound=2**128)
R = Integers(p)
g = R(2)

# tests
for _ in range(128):
    x = randrange(0, p)
    y = randrange(0, p)
    assert g**x * g**y == g**(x+y)
    assert (g**x)**y == g**(x*y)

F = [ randrange(1,p) for _ in range(0,n-1) ]
print "F =", F

def f(x):
    val = 0
    for i in range(n-1):
        val += F[i] * x**(i+1)
    return val

pat = [0] * (n - 1) + ['*']
x   = [0] * (n - 1) + [0]

print "pat = ", pat
print "x =", x

h = []
hp = []
for i in range(n):
    h.append([])
    hp.append([])
    for b in range(2):
        if pat[i] == '*' or pat[i] == b:
            pt = 2*(i+1) + b

            h[i].append(g**f(pt))
            hp[i].append(f(pt))
        else:
            r = randrange(1,p)
            h[i].append(g**r)
            hp[i].append(r)

print "h =", h
print "hp =", hp

def c(i, x):
    val = 1
    for j in range(n):
        if i != j:
            pi = 2*(i+1) + x[i]
            pj = 2*(j+1) + x[j]
            val *= (-pj) / (pi - pj)
    return val

val  = prod([ h[i][x[i]] ** (int(c(i,x))) for i in range(n) ])
valp = sum([ hp[i][x[i]] *  c(i,x) % p for i in range(n) ]) % p

print val, valp
