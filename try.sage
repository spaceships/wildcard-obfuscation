from random import randrange

n = 2
#  p = random_prime(2^(n+1), lbound=2^n)
p = 97
R = Integers(p)
g = R(2)

#  F = [ randrange(1,p) for _ in range(0,n-1) ]
F = [ 2 ] * (n-1)
print "F =", F

def f(x):
    val = R(0)
    for i in range(n-1):
        val += R(F[i]) * x**R(i+1)
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
            pt = R( 2*(i+1)+b )
            print "point at ", pt

            h[i].append(R(f(pt)))
            hp[i].append(f(pt))
        else:
            h[i].append(0)
            hp[i].append(0)

print "h =", h
print "hp =", hp

def c(i, x):
    val = R(1)
    for j in range(n):
        if i != j:
            pi = 2*(i+1) + x[i]
            pj = 2*(j+1) + x[j]
            val *= R(-pj) / R(pi - pj)
    return val

print "c_i's =", [ c(i, x) for i in range(n) ]

val  = prod([ h[i][x[i]] ** c(i,x) for i in range(n) ])
valp = sum([ hp[i][x[i]] *  c(i,x) % p for i in range(n) ]) % p

print val, valp
