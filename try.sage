from random import randrange

n = 100
#  p = random_prime(2**(n+1), lbound=2**n)
#  p = 97
#  p = random_prime(2**129, lbound=2**128)

q = 2**80
p = 2*q + 1

while not is_prime(p) or not is_prime(q):
    q = next_prime(q)
    p = 2*q + 1

G = Integers(p)
g = G(2)

#  ec = EllipticCurve(GF(2**255-19), [0,486662,0,1,0])
#  g = ec.lift_x(9)

print "p =", p
print "q =", q

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
x   = [0] * (n - 1) + [1]

print "pat = ", pat

def point(i,j):
    return 2*(i+1) + j

h = []
hp = []
for i in range(n):
    h.append([])
    hp.append([])
    for b in range(2):
        if pat[i] == '*' or pat[i] == b:
            pt = point(i, b)

            h[i].append(g**f(pt))
            hp[i].append(f(pt))
        else:
            r = randrange(1,p)
            #  r = 0
            h[i].append(G(r))
            hp[i].append(r)

#  print "h =", h
#  print "hp =", hp
print "x =", x

def c(i, x):
    val = 1
    for j in range(n):
        if i != j:
            pi = point(i, x[i])
            pj = point(j, x[j])
            tmp = (-pj) / (pi - pj)
            val *= tmp
    return val

val = G(1)
valp = 0
for i in range(n):
    l = c(i,x)
    #  print "l={}".format(l)
    val *= h[i][x[i]] ** (l.numer() * inverse_mod(l.denom(), q))

    valp += hp[i][x[i]] * c(i,x)
    valp %= p

    #  print val, valp

print val, valp
