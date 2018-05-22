from random import randrange

n = 6
#  p = random_prime(2**(n+1), lbound=2**n)
#  p = 97
#  p = random_prime(2**129, lbound=2**128)
p = 99347
R = Integers(p)
g = R(2)

print "p =", p

# tests
for _ in range(128):
    x = randrange(0, p)
    y = randrange(0, p)
    assert g**x * g**y == g**(x+y)
    assert (g**x)**y == g**(x*y)

#  F = [ randrange(1,p) for _ in range(0,n-1) ]
F = [ 1 for _ in range(0,n-1) ]
print "F =", F

def f(x):
    val = 0
    for i in range(n-1):
        val += F[i] * x**(i+1)
    return val

pat = [0] * (n - 1) + ['*']
x   = [0] * (n - 1) + [1]

print "pat = ", pat

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
            #  r = randrange(1,p)
            r = 0
            h[i].append(r)
            hp[i].append(r)

print "h =", h
#  print "hp =", hp
print "x =", x

def c(i, x):
    val = 1
    for j in range(n):
        if i != j:
            pi = 2*(i+1) + x[i]
            pj = 2*(j+1) + x[j]
            tmp = (-pj) / (pi - pj)
            #  print tmp
            val *= tmp
    return val

val = 1
#  valp = 0
for i in range(n):
    print "c({},x) = {}".format(i, c(i,x))
    val *= h[i][x[i]] ** int(c(i,x)) % p
    val %= p
    print val
    #  valp += hp[i][x[i]] * c(i,x)
    #  valp %= p

print val
